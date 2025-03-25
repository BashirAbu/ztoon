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
// 5. Verify the module, add it to a LLJIT instance, lookup "main", run it
// and
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
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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

TEST(CodeGen_ForLoopBreak)
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
                if a > 20 {
                    break;
                }
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 21, "Result should be 21");
}

TEST(CodeGen_ForLoopContinue)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Sum numbers 0 through 9: expected result is 45.
    std::string source = R"(
        fn main() -> i32 {
            a: i32 = 0;
            for i: i32 = 0; i < 10; i++ {
                if a > 10 {
                    continue;
                }
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 15, "Result should be 15");
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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

// Test 3: Nested loops (complex computation).
TEST(CodeGen_NestedLoopsBreakContinue)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        fn main() -> i32 {
            a: i32 = 0;
            for i: i32 = 0; i < 10; i++ {
                a += i;
                while a < 5 {
                    a++;
                    if a == 2 {
                        continue;
                    }

                    if a == 4
                    {
                        break;
                    }
                }

                if i != a {
                    continue;
                }

                if ( a > 15)
                {
                    break;
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 49, "Result should be 49");
} // Test 4: While loop.
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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

TEST(CodeGen_WhileLoopBreak)
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
                if b == 3
                {
                    break;
                }
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 3, "While loop sum should equal 3");
}
TEST(CodeGen_WhileLoopContinue)
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
                b++;
                if b == 3
                {
                    continue;
                }
                a = a + b;
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 11, "While loop sum should equal 11");
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
TEST(CodeGen_UnarySizeOf)

{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source =
        "fn main() -> i32 { arr: i32[10]; ret sizeof(i32[10]) as i32; }";
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
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 40, "Value should be 40");
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
TEST(CodeGen_DerefPTR)
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
TEST(CodeGen_UninitializedGlobalVars)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(
        gA: i32;
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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

TEST(CodeGen_ArrayDecl)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[4];
            ret 0;
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
TEST(CodeGenArrayCopy)
{
    std::string source = R"(
        fn main() -> u32
        {
            arr: u32[3] = {1,  2, 3};
            arr2: u32[3] = arr;
            arr[1] = 2222;
            arr2[1] = arr[1] * arr2[1];
            ret arr2[1];
            
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 4444, "Value should be 4444");
}
TEST(CodeGenArrayReference)
{
    std::string source = R"(
        fn main() -> u32
        {
            arr: u32[3] = {1,  2, 3};
            arr2: u32[3]* = &arr;
            arr[1] = 2222;
            arr[1]--;
            ret (*arr2)[1];
            
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 2221, "Value should be 2221");
}

TEST(CodeGenArrayInitializerListReAssignment)
{
    std::string source = R"(
        fn main() -> u32
        {
            arr: u32[3] = {1,  2, 3};
            arr = {4, 5, 4};
            ret arr[1];
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 5, "Value should be 5");
}
TEST(CodeGenFunctionParamterArrayTypeByValueAndRetArrayTypeByValue)
{
    std::string source = R"(

        fn array_stuff(a: u32[3]) -> u32[3]
        {
            a[2] = 99;
                        ret a;
        }
        
        fn main() -> u32
        {
            arr: u32[3] = {1,  2, 3};
            v: u32 = (array_stuff(arr))[2];
            ret v;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 99, "Value should be 5");
}
TEST(CodeGenFunctionParamterArrayTypeByReferenceAndRetArrayTypeByReference)
{
    std::string source = R"(

        fn array_stuff(a: u32[3]*) -> u32[3]
        {
            (*a)[2] = 99;
            ret *a;
        }
        
        fn main() -> u32
        {
            arr: u32[3] = {1,  2, 3};
            array_stuff(&arr);
            ret arr[2];
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 99, "Value should be 5");
}
TEST(CodeGen_ArrayDeclEmptySizeExpression)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[] = { 1, 3};
            arr2: i32[][2] = {arr, {3,5}};
            ret arr[1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 3, "Value should be 0");
}

TEST(CodeGen_ArrayToPointerType)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[2] = { 1,5};
            ptr: i32* = arr as i32*;
            ret ptr[1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 5, "Value should be 5");
}
TEST(CodeGen_ArrayDeclWithInitializerList)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[4] = {1,4,6,7};
            ret 0;
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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
TEST(CodeGen_ArrayDecl2D)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[2][2] = {{1,3},{5, 9}};
            ret arr[1][1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 9, "Value should be 9");
}
TEST(CodeGen_ArrayDecl3D)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[2][2][2] = {{{1,2},{3,4}},{{1212,6},{7,8}}};

            ret arr[0][1][1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();

    // ASSERT_EQ(r, 6, "Value should be 6");
}
TEST(CodeGen_SubscriptReadValue)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[4] = {1,4,6,7};
            ret arr[2];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 6, "Value should be 6");
}

TEST(CodeGen_SubscriptWriteValue)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        fn main() -> i32
        {
            arr: i32[4] = {1,4,6,7};
            arr[2] = 33333;
            ret arr[2];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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

TEST(CodeGen_GlobalArrayDecl)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        arr : i32[3];

        fn main() -> i32
        {

            ret arr[2];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

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

TEST(CodeGen_GlobalArrayDeclWithInitializerList)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        arr : i32[3] = {1, 4, 8};

        fn main() -> i32
        {

            ret arr[1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 4, "Value should be 4");
}

TEST(CodeGen_GlobalArray2DDeclWithInitializerList)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::string source = R"(

        arr : i32[2][2] = {{5,7},{2,9}};

        fn main() -> i32
        {
            ret arr[1][1];
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
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    int r = Fp();
    ASSERT_EQ(r, 9, "Value should be 9");
}
TEST(CodeGenStrings)
{
    Lexer lexer;
    std::string source =
        " fn printf(str: readonly i8*, ...) -> i32;"
        " fn main()"
        "  {"
        "       str: i8[] = {'b', 's', 'r', '\\n', '\\0'};"
        "       raw_str : i8* = R\"(\\n\\n\\nhi\\n\\n\\n\\n)\";"
        " printf(\"\\\\Hello \t\\\"World\\\" "
        "\\\'from\\\' \\n "
        "%s\", raw_str);"
        "}";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    Fp();
}
TEST(CodeGenFunctionPrototypeVarArgs)
{
    Lexer lexer;
    std::string source = R"(
        fn printf(str: readonly i8*, ...) -> i32;
        fn main()
        {
            str: i8[] = {'b', 's', 'r', '\n', '\0'};
            printf("\\Hello \t\"World\" \'from\' \n%s", str as i8*);
        })";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    Fp();
}

TEST(CodeGenHeapAllocation)
{
    Lexer lexer;
    std::string source = R"(
        fn printf(str: readonly i8*, ...) -> i32;
        fn malloc(size: u64) -> i8*;

        // struct vector
        // {
        //     x: f32 = 0.0;
        //     y: f32 = 0.0;

        //     fn Len() ->f32
        //     {
        //         return sqrt( x * x - y * y );
        //     }
        // }
        
        fn main()
        {
            buffer: i32* = malloc(4 * 12) as i32*;
            for i: i32 = 0; i < 12; i++ {
                buffer[i] = i;
                if i == 5
                {
                    continue;
                }else if i == 10
                {
                    break;
                
                }
                printf("%d\n", buffer[i]);
            }
            printf("%d", buffer[11]);
            
        })";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    Fp();
}
TEST(CodeGenRandomStuff)
{
    Lexer lexer;
    std::string source = R"(
        fn printf(str: readonly i8*, ...) -> i32;
        fn strcmp(str1: readonly i8*, str2: readonly i8*) -> i32;
        fn main()
        {
            str1 : i8* = "bashir";
            str2 : i8* = "bashir";
            if !(strcmp(str1, str2) as bool) {
                printf("Same");
            }
            else
            {
                printf("Not Same");
            }
        })";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    CodeGen codeGen(sa, "x86_64-pc-windows-msvc");
    codeGen.GenIR();
    if (llvm::verifyModule(*codeGen.module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }
    // codeGen.module->print(llvm::outs(), nullptr);

    llvm::ExitOnError err;
    auto JIT = err(llvm::orc::LLJITBuilder().create());
    llvm::orc::ThreadSafeModule TSM(std::move(codeGen.module),
                                    std::move(codeGen.ctx));
    err(JIT->addIRModule(std::move(TSM)));
    auto Sym = err(JIT->lookup("main"));
    auto *Fp = (int (*)())Sym.getValue();
    Fp();
}
