#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

#include "code_gen/code_gen.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

TEST(CodeGen)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source =
        R"(
        a: i32 = 0;
          for i: i32 = 0; i < 10; i++
          {
                for i: i32 = 0; i < 10; ++i {
                    for i: i32 = 0; i < 10; ++i {
                        b: i32 = 1;
                        while b < 123 {
                            a += i * b++;
                        }
                   }
               }
          }
               
           )";
    Lexer lexer;
    lexer.Tokenize(source, "source.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();

    CodeGen codeGen(sa);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);

    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);

    llvm::BasicBlock *entryBlock =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);

    codeGen.irBuilder->SetInsertPoint(entryBlock);
    // code gen here for now.
    codeGen.GenIR();
    IRVariable *var = codeGen.GetIRVariable("a");
    llvm::Value *v =
        codeGen.irBuilder->CreateLoad(var->irType.type, var->aInsta);
    codeGen.irBuilder->CreateRet(v);
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Error\n";
    }
    codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));

    err(JIT->addIRModule(std::move(TSM)));

    auto Sym = err(JIT->lookup("main"));

    auto *Fp = (int (*)())Sym.getValue();

    int r = Fp();
}

// Test for integer arithmetic:
// Program source:
//   x: i32 = 10;
//   y: i32 = 20;
//   z: i32 = x + y;
TEST(CodeGenArithmeticTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source =
        R"(
            x: i32 = 10;
            y: i32 = 20;
            z: i32 = x + y;
        )";
    Lexer lexer;
    lexer.Tokenize(source, "arith.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    // Retrieve variable z and generate a return from its stored value.
    IRVariable *zVar = codeGen.GetIRVariable("z");
    llvm::Value *loadedValue =
        codeGen.irBuilder->CreateLoad(zVar->irType.type, zVar->aInsta);
    codeGen.irBuilder->CreateRet(loadedValue);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 30, "Expected x + y to equal 30");
}

// Test for float arithmetic with casting:
// Program source:
//   pi: f32 = 3.14;
//   r: i32 = 7;
//   a: f32 = pi * ( r * r ) as f32;
TEST(CodeGenFloatArithmeticTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source =
        R"(
            pi: f32 = 3.14;
            r: i32 = 7;
            a: f32 = pi * ( r * r ) as f32;
        )";
    Lexer lexer;
    lexer.Tokenize(source, "float_arith.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getFloatTy(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    // Retrieve variable a and return its stored value.
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadedValue =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadedValue);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (float (*)())Sym.getValue();
    float result = mainFuncPtr();

    // Expected result: 3.14 * (7*7) = 3.14 * 49 = 153.86 (approximately).
    // We'll compare the result rounded to two decimals.
    int rounded = static_cast<int>(result * 100);
    ASSERT_EQ(rounded, 15386, "Expected area (a) to be approximately 153.86");
}

//------------------------------------------------------------
// Test: Binary Minus Operator
// Program:
//   x: i32 = 10;
//   y: i32 = 3;
//   z: i32 = x - y;
TEST(CodeGenBinaryMinusTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 10;
        y: i32 = 3;
        z: i32 = x - y;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "binary_minus.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    // Return variable z.
    auto *zVar = codeGen.GetIRVariable("z");
    llvm::Value *loadZ =
        codeGen.irBuilder->CreateLoad(zVar->irType.type, zVar->aInsta);
    codeGen.irBuilder->CreateRet(loadZ);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 7, "Expected 10 - 3 to equal 7");
}

//------------------------------------------------------------
// Test: Binary Multiplication Operator
// Program:
//   x: i32 = 6;
//   y: i32 = 7;
//   z: i32 = x * y;
TEST(CodeGenBinaryMulTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 6;
        y: i32 = 7;
        z: i32 = x * y;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "binary_mul.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *zVar = codeGen.GetIRVariable("z");
    llvm::Value *loadZ =
        codeGen.irBuilder->CreateLoad(zVar->irType.type, zVar->aInsta);
    codeGen.irBuilder->CreateRet(loadZ);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 42, "Expected 6 * 7 to equal 42");
}

//------------------------------------------------------------
// Test: Binary Division Operator
// Program:
//   x: i32 = 20;
//   y: i32 = 4;
//   z: i32 = x / y;
TEST(CodeGenBinaryDivTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 20;
        y: i32 = 4;
        z: i32 = x / y;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "binary_div.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *zVar = codeGen.GetIRVariable("z");
    llvm::Value *loadZ =
        codeGen.irBuilder->CreateLoad(zVar->irType.type, zVar->aInsta);
    codeGen.irBuilder->CreateRet(loadZ);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 5, "Expected 20 / 4 to equal 5");
}
// Test binary remainder: 10 % 3 = 1.
TEST(CodeGenBinaryRemainderTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 10 % 3;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "bin_rem.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 1, "Expected 10 % 3 to equal 1");
}

//===----------------------------------------------------------------------===//
// Binary Bitwise Operators (integers)
//===----------------------------------------------------------------------===//

// Test bitwise AND: 6 & 3 = 2.
TEST(CodeGenBinaryBitwiseAndTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 6 & 3;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "bin_band.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 2, "Expected 6 & 3 to equal 2");
}

// Test bitwise OR: 6 | 3 = 7.
TEST(CodeGenBinaryBitwiseOrTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 6 | 3;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "bin_bor.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 7, "Expected 6 | 3 to equal 7");
}

// Test bitwise XOR: 6 ^ 3 = 5.
TEST(CodeGenBinaryBitwiseXorTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 6 ^ 3;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "bin_bxor.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();

    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 5, "Expected 6 ^ 3 to equal 5");
}
//------------------------------------------------------------
// Test: Binary Shift Left Operator (<<)
// Program:
//   a: i32 = 1 << 2;
// Expected: 1 << 2 = 4.
TEST(CodeGenBinaryShiftLeftTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 1 << 2;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "shift_left.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 4, "Expected 1 << 2 to equal 4");
}

//------------------------------------------------------------
// Test: Binary Shift Right Operator (>>)
// Program:
//   a: i32 = 8 >> 2;
// Expected: 8 >> 2 = 2.
TEST(CodeGenBinaryShiftRightTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 8 >> 2;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "shift_right.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 2, "Expected 8 >> 2 to equal 2");
}

//------------------------------------------------------------
// Comparison Operator Tests (return bool as i1)

// Test: Less Than (<)
// Program:
//   a: bool = 3 < 5;
// Expected: true.
TEST(CodeGenComparisonLessTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 3 < 5;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_less.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    // For booleans, use i1 return type.
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected 3 < 5 to yield true");
}

// Test: Less Than or Equal (<=)
// Program:
//   a: bool = 5 <= 5;
// Expected: true.
TEST(CodeGenComparisonLessEqualTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 5 <= 5;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_less_equal.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected 5 <= 5 to yield true");
}

// Test: Greater Than (>)
// Program:
//   a: bool = 7 > 2;
// Expected: true.
TEST(CodeGenComparisonGreaterTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 7 > 2;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_greater.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected 7 > 2 to yield true");
}

// Test: Greater Than or Equal (>=)
// Program:
//   a: bool = 5 >= 6;
// Expected: false.
TEST(CodeGenComparisonGreaterEqualTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 5 >= 6;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_greater_equal.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, false, "Expected 5 >= 6 to yield false");
}

// Test: Equality (==)
// Program:
//   a: bool = 3 == 3;
// Expected: true.
TEST(CodeGenComparisonEqualEqualTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 3 == 3;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_eq_eq.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected 3 == 3 to yield true");
}

// Test: Inequality (!=)
// Program:
//   a: bool = 3 != 4;
// Expected: true.
TEST(CodeGenComparisonNotEqualTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = 3 != 4;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cmp_not_eq.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected 3 != 4 to yield true");
}

//------------------------------------------------------------
// Logical Operator Tests (booleans)
//===---------------------------------------------------------//

// Test: Logical OR (||)
// Program:
//   a: bool = true || false;
// Expected: true.
TEST(CodeGenLogicalOrTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = true || false;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "logical_or.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, true, "Expected true || false to yield true");
}

// Test: Logical AND (&&)
// Program:
//   a: bool = true && false;
// Expected: false.
TEST(CodeGenLogicalAndTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: bool = true && false;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "logical_and.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (bool (*)())Sym.getValue();
    bool result = mainPtr();
    ASSERT_EQ(result, false, "Expected true && false to yield false");
}
//------------------------------------------------------------
// Test: Unary Negation Operator (-)
// Program:
//   x: i32 = -10;
TEST(CodeGenUnaryNegationTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = -10;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_neg.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *xVar = codeGen.GetIRVariable("x");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, -10, "Expected -10 for unary negation");
}

//------------------------------------------------------------
// Test: Unary Bitwise NOT Operator (~)
// Program:
//   x: i32 = ~0;
TEST(CodeGenUnaryBitwiseNotTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = ~0;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_bitnot.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *xVar = codeGen.GetIRVariable("x");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, -1, "Expected ~0 to produce -1 for a 32-bit integer");
}

//------------------------------------------------------------
// Test: Unary Decrement Operator (--)
// Program:
//   x: i32 = --10;
TEST(CodeGenUnaryDecrementTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 10;
        x--;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_decrement.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *xVar = codeGen.GetIRVariable("x");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 9, "Expected --10 to produce 9");
}

//------------------------------------------------------------
// Test: Unary Increment Operator (++)
// Program:
//   x: i32 = ++10;
TEST(CodeGenUnaryIncrementTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 10;
        a: i32 = x++;
        a = x;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_increment.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *xVar = codeGen.GetIRVariable("a");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 11, "Expected ++10 to produce 11");
}

//------------------------------------------------------------
// Test: Unary Logical NOT Operator (!)
// Program:
//   b: bool = !false;
TEST(CodeGenUnaryLogicalNotTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        b: bool = !false;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_logical_not.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    // For bool, use i1 return type.
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt1Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *bVar = codeGen.GetIRVariable("b");
    llvm::Value *loadB =
        codeGen.irBuilder->CreateLoad(bVar->irType.type, bVar->aInsta);
    codeGen.irBuilder->CreateRet(loadB);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (bool (*)())Sym.getValue();
    bool result = mainFuncPtr();
    ASSERT_EQ(result, true, "Expected !false to yield true");
}

//------------------------------------------------------------
// Test: Unary SIZEOF Operator
// Program:
//   x: i32 = sizeof(10);
TEST(CodeGenUnarySizeofTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = sizeof(10) as i32;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_sizeof.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    // For sizeof, our CodeGen returns an i32.
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *xVar = codeGen.GetIRVariable("x");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int32_t (*)())Sym.getValue();
    int32_t result = mainFuncPtr();
    // Expect size of i32 to be 4 bytes.
    ASSERT_EQ(result, 4, "Expected sizeof(i32) to yield 4 bytes");
}

//------------------------------------------------------------
// Test: Casting Int to Float
// Program:
//   x: i32 = 42;
//   y: f32 = x as f32;
TEST(CodeGenCastIntToFloatTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 42;
        y: f32 = x as f32;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cast_int_to_float.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getFloatTy(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    auto *yVar = codeGen.GetIRVariable("y");
    llvm::Value *loadY =
        codeGen.irBuilder->CreateLoad(yVar->irType.type, yVar->aInsta);
    codeGen.irBuilder->CreateRet(loadY);

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
        llvm::errs() << "Module verification failed\n";
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (float (*)())Sym.getValue();
    float result = mainFuncPtr();
    ASSERT_EQ(static_cast<int>(result * 100), 4200,
              "Expected 42.0 after casting int 42 to f32");
}

//------------------------------------------------------------
// Test: Casting Float to Int
// Program:
//   x: f32 = 3.14;
//   y: i32 = x as i32;
TEST(CodeGenCastFloatToIntTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: f32 = 3.14;
        y: i32 = x as i32;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "cast_float_to_int.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();

    CodeGen codeGen(semAnalyzer);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
}

// Program source:
//   if (1 < 2) {
//       a: i32 = 42;
//   } else {
//       a: i32 = 0;
//   }
// main() returns the value of 'a' (expected: 42)
TEST(CodeGenIfElseTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a:i32 = 0;
        if (1 < 2) {
            a  = 42;
        } else {
            a= 0;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "if_else.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 42, "If-Else: expected a to be 42");
}

// Test 2: If-else with false condition.
// Program source:
//   if (2 < 1) {
//       a: i32 = 42;
//   } else {
//       a: i32 = 7;
//   }
// Expected: a is set to 7.
TEST(CodeGenIfElseFalseTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 0;
        if (2 < 1) {
            a = 42;
        } else {
            a= 7;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "if_else_false.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 7, "If-Else (false branch): expected a to be 7");
}

// Test 3: If - Else If - Else chain.
// Program source:
//   if (2 < 1) {
//       a: i32 = 100;
//   } else if (3 < 5) {
//       a: i32 = 55;
//   } else {
//       a: i32 = 0;
//   }
// Expected: a is set to 55.
TEST(CodeGenElseIfTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = 0;
        if (2 < 1) {
            a = 100;
        } else if (3 < 5) {
            a = 55;
        } else {
            a = 0;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "elseif.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 55, "Else-If: expected a to be 55");
}

// Test 4: Nested if statements.
// Program source:
//   x: i32 = 10;
//   if (x < 20) {
//       if (x < 5) {
//           x = 0;
//       } else {
//           x = x + 5;
//       }
//   }
// Expected: x becomes 15.
TEST(CodeGenNestedIfTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        x: i32 = 10;
        if (x < 20) {
            if (x < 5) {
                x = 0;
            } else {
                x = x + 5;
            }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "nested_if.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *xVar = codeGen.GetIRVariable("x");
    llvm::Value *loadX =
        codeGen.irBuilder->CreateLoad(xVar->irType.type, xVar->aInsta);
    codeGen.irBuilder->CreateRet(loadX);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 15, "Nested If: expected x to be 15");
}

// Test 5: If statement with nested block statement.
// Program source:
//   if (true) {
//       {
//           a: i32 = 123;
//       }
//   } else {
//       a: i32 = 0;
//   }
// Expected: a is set to 123.
TEST(CodeGenNestedBlockTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a:i32 = 0;
        if (true) {
            {
                a = 123;
            }
        } else {
            a = 0;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "nested_block.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainFuncPtr = (int (*)())Sym.getValue();
    int result = mainFuncPtr();
    ASSERT_EQ(result, 123, "Nested Block: expected a to be 123");
}

//===----------------------------------------------------------------------===//
// Ternary Expression Tests
//===----------------------------------------------------------------------===//

// Test 1: Simple ternary expression with integer branches.
//   a: i32 = (1 < 2) ? 100 : 200;
// Expected result: 100.
TEST(CodeGenTernaryTrueTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = (1 < 2) ? 100 : 200;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "ternary_true.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 100, "Ternary (true branch): expected 100");
}

// Test 2: Simple ternary expression with false condition.
//   a: i32 = (2 < 1) ? 100 : 200;
// Expected result: 200.
TEST(CodeGenTernaryFalseTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = (2 < 1) ? 100 : 200;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "ternary_false.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 200, "Ternary (false branch): expected 200");
}

// Test 3: Nested ternary expression.
//   a: i32 = (1 < 2) ? ((3 < 4) ? 300 : 400) : 500;
// Expected: Outer condition true, inner condition true, so result 300.
TEST(CodeGenNestedTernaryTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = (1 < 2) ? ((3 < 4) ? 300 : 400) : 500;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "nested_ternary.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 300, "Nested ternary: expected 300");
}

// Test 4: Ternary expression combined with addition.
//   a: i32 = ((2 < 1) ? 100 : 200) + 10;
// Expected: false condition yields 200, plus 10 equals 210.
TEST(CodeGenTernaryWithAdditionTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: i32 = ((2 < 1) ? 100 : 200) + 10;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "ternary_add.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getInt32Ty(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (int (*)())Sym.getValue();
    int result = mainPtr();
    ASSERT_EQ(result, 210, "Ternary with addition: expected 210");
}

// Test 5: Ternary expression with floating–point result.
//   a: f32 = (1 < 2) ? 3.14 : 2.71;
// Expected: result approximately 3.14.
TEST(CodeGenTernaryFloatTest)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        a: f32 = (1 < 2) ? 3.14 : 2.71;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "ternary_float.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer semAnalyzer(stmts);
    semAnalyzer.Analize();
    CodeGen codeGen(semAnalyzer);

    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getFloatTy(), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", *codeGen.module);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*codeGen.ctx, "entry", mainFunc);
    codeGen.irBuilder->SetInsertPoint(entry);
    codeGen.GenIR();
    IRVariable *aVar = codeGen.GetIRVariable("a");
    llvm::Value *loadA =
        codeGen.irBuilder->CreateLoad(aVar->irType.type, aVar->aInsta);
    codeGen.irBuilder->CreateRet(loadA);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *mainPtr = (float (*)())Sym.getValue();
    float result = mainPtr();
    // Compare rounded to two decimals.
    int rounded = static_cast<int>(result * 100);
    ASSERT_EQ(rounded, 314,
              "Ternary float: expected result approximately 3.14");
}
