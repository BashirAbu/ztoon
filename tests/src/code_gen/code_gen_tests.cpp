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
   pi: f32 = 3.14;
   r: i32 = 7;
   a: f32 = pi * ( r * r ) as f32;
       
    )";
    Lexer lexer;
    lexer.Tokenize(source, "source.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();

    CodeGen codeGen(sa);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.irBuilder->getFloatTy(), false);

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

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));

    err(JIT->addIRModule(std::move(TSM)));

    auto Sym = err(JIT->lookup("main"));

    auto *Fp = (float (*)())Sym.getValue();

    float r = Fp();
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
        x: i32 = --10;
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
        x: i32 = ++10;
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
