#include "code_gen/code_gen.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <string>

// Each test follows this pattern:
// 1. Initialize LLVM native target support.
// 2. Provide a source string.
// 3. Run lexing, parsing, semantic analysis.
// 4. Generate IR via CodeGen.
// 5. Verify the module, add it to a LLJIT instance, lookup "main", run it and
// check the result.

// Test 1: Simple arithmetic.
TEST(CodeGen_SimpleArithmetic)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = "fn main() -> i32 { ret 1 + 2 * 3; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Error in module\n";
    }
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 7, "1 + 2 * 3 should equal 7");
}

// Test 2: For loop summing numbers.
TEST(CodeGen_ForLoop)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Sum numbers 0 through 9: expected result is 45.
    std::string source = R"(
        fn main() -> i32 {
            a: i32 = 0;
            for i: i32 = 0; i < 10; i++ {
                a = a + i;
            }
            ret a;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 45, "Sum of 0 to 9 should equal 45");
}

// Test 3: Nested loops (complex computation).
TEST(CodeGen_NestedLoops)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        fn main() -> i32 {
            a: i32 = 0;
            for i: i32 = 0; i < 10; i++ {
                for j: i32 = 0; j < 10; j++ {
                    for k: i32 = 0; k < 10; k++ {
                        b: i32 = 1;
                        while b < 123 {
                            a += i * b;
                            b++;
                        }
                    }
                }
            }
            ret a;
        }
    )";
    // Expected result computed externally: 33763500
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 33763500, "Nested loops should compute 33763500");
}

// Test 4: While loop.
TEST(CodeGen_WhileLoop)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Compute a sum with a while loop: sum of 1+2+3+4 = 10.
    std::string source = R"(
        fn main() -> i32 {
            a: i32 = 0;
            b: i32 = 1;
            while b < 5 {
                a = a + b;
                b++;
            }
            ret a;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 10, "While loop sum should equal 10");
}

// Test 5: If statement.
TEST(CodeGen_IfStatement)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // If condition true branch should return 100.
    std::string source = R"(
        fn main() -> i32 {
            if 1 < 2 {
                ret 100;
            } else {
                ret 200;
            }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 100, "If statement should return 100");
}

// Test 6: Ternary expression.
TEST(CodeGen_TernaryExpression)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = "fn main() -> i32 { ret (true ? 50 : 60); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 50, "Ternary expression should return 50");
}

// Test 7: Function call.
TEST(CodeGen_FunctionCall)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Define a helper function foo and then call it from main.
    std::string source = R"(
        fn foo() -> i32 { ret 10; }
        fn main() -> i32 { ret foo() + 5; }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 15, "Function call should return 15");
}

// Test 8: Unary minus.
TEST(CodeGen_UnaryMinus)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = "fn main() -> i32 { ret -10; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, -10, "Unary minus should yield -10");
}

// Test 9: Compound assignment.
TEST(CodeGen_CompoundAssignment)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = "fn main() -> i32 { a: i32 = 5; a += 3; ret a; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 8, "Compound assignment should yield 8");
}

// Test 10: Float arithmetic.
TEST(CodeGen_FloatArithmetic)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Simple float addition.
    std::string source = "fn main() -> f32 { ret 2.0 + 3.0; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (float (*)())Sym.getValue();
    float r = Fp();
    ASSERT_EQ(r, 5.0f, "Float arithmetic should yield 5.0");
}

// Test 11: Mixed arithmetic with implicit cast.
TEST(CodeGen_MixedCast)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // 3 (i32) + 2.0 (f32) should be implicitly cast and computed as float.
    std::string source = "fn main() -> f32 { ret 3 as f32 + 2.0; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (float (*)())Sym.getValue();
    float r = Fp();
    ASSERT_EQ(r, 5.0f, "Mixed arithmetic should yield 5.0");
}

// Test 12: Boolean operations.
TEST(CodeGen_BooleanOps)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = "fn main() -> bool { ret true && false || true; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (bool (*)())Sym.getValue();
    bool r = Fp();
    ASSERT_EQ(r, true, "Boolean operations should yield true");
}

// Test 13: Shift operations.
TEST(CodeGen_ShiftOps)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // (8 >> 1) is 4 and (1 << 3) is 8 so result is 12.
    std::string source = "fn main() -> i32 { ret (8 >> 1) + (1 << 3); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 12, "Shift operations should yield 12");
}

// Test 14: Bitwise operations.
TEST(CodeGen_BitwiseOps)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // (5 & 3) is 1, (4 ^ 1) is 5, and (1 | 5) is 5.
    std::string source = "fn main() -> i32 { ret (5 & 3) | (4 ^ 1); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 5, "Bitwise operations should yield 5");
}
TEST(CodeGen_Recursion)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        fn fact(n: i32) -> i32 {
            if n <= 1 { ret 1; } else { ret n * fact(n - 1); }
        }
        fn main() -> i32 { ret fact(5); }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 120, "Recursion: fact(5) should equal 120");
}

TEST(CodeGen_PTR)
{

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        
        fn main() -> u64
         {
             a: i32 = 422;
             b: i32* = &a;
             c: i32** = &b;
             ret c as u64;
         }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (size_t (*)())Sym.getValue();
    size_t r = Fp();
    int deref = **((size_t **)r);
    ASSERT_EQ(deref, 422, "value should be 422")
}
TEST(CodeGen_FNPTR)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        fn main() -> i32
        {
            a: i32;
            aptr : i32* = &a;
            a_ptr_int : u64 = aptr as u64;
            *(a_ptr_int as i32*) = 33333;
            ret a;
        }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 33333, "Value should be 33333");
}

TEST(CodeGen_UninitializedVariables)
{

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        fn main() -> i32
        {
            a: i32;
            ret a;
        }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 0, "Value should be 0");
}

TEST(CodeGen_InitializedGlobalVars)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        gA: i32 = 12;
        fn main() -> i32
        {
            ret gA;
        }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 12, "Value should be 12");
}
TEST(CodeGen_GlobalVarsIniailzedWithReadOnly)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        rdOnly : readonly i32 = 1234;
        gA: i32 = rdOnly;
        ptrGA: i32* = &gA;
        fn main() -> i32
        {
            ret gA;
        }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 1234, "Value should be 1234");
}
