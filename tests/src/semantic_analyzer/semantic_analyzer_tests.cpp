#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

// Test 1: Simple arithmetic expression.
TEST(SemanticArithmeticTest)
{
    std::string source = "fn main() -> i32 { ret 42 + 2; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 2: Boolean expression using logical AND.
TEST(SemanticBooleanAndTest)
{
    std::string source = "fn main() -> bool { ret true && false; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "bool",
              "Return type should be bool");
}

// Test 3: Comparison expression.
TEST(SemanticComparisonTest)
{
    std::string source = "fn main() -> bool { ret 3 < 5; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "bool",
              "Return type should be bool");
}

// Test 4: Variable declaration and use.
TEST(SemanticVarDeclarationTest)
{
    std::string source = "fn main() -> i32 { x: i32 = 100; ret x; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 5: Compound assignment.
TEST(SemanticCompoundAssignmentTest)
{
    std::string source = "fn main() -> i32 { x: i32 = 10; x += 5; ret x; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 6: Function call.
TEST(SemanticFunctionCallTest)
{
    std::string source =
        "fn foo() -> i32 { ret 10; } fn main() -> i32 { ret foo(); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    // There should be two global declarations.
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *foo = dynamic_cast<FnStatement *>(ast[0]);
    auto *mainFn = dynamic_cast<FnStatement *>(ast[1]);
    ASSERT_NE(foo, nullptr, "Expected first function to be 'foo'");
    ASSERT_NE(mainFn, nullptr, "Expected second function to be 'main'");
    ASSERT_EQ(foo->GetReturnDatatype()->ToString(), "i32",
              "Function foo return type should be i32");
    ASSERT_EQ(mainFn->GetReturnDatatype()->ToString(), "i32",
              "Function main return type should be i32");
}

// Test 7: Unary minus.
TEST(SemanticUnaryMinusTest)
{
    std::string source = "fn main() -> i32 { ret -5; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 8: Cast expression.
TEST(SemanticCastTest)
{
    std::string source = "fn main() -> i32 { ret 42 as i32; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 9: Ternary expression.
TEST(SemanticTernaryTest)
{
    std::string source = "fn main() -> i32 { ret true ? 1 : 2; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 10: Grouping expression.
TEST(SemanticGroupingTest)
{
    std::string source = "fn main() -> i32 { ret (1 + 2); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
}

// Test 11: Logical OR.
TEST(SemanticLogicalOrTest)
{
    std::string source = "fn main() -> bool { ret true || false; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "bool",
              "Return type should be bool");
}

// Test 12: Logical AND.
TEST(SemanticLogicalAndTest)
{
    std::string source = "fn main() -> bool { ret true && true; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "bool",
              "Return type should be bool");
}

// Test 13: Function with parameters.
TEST(SemanticFunctionWithParametersTest)
{
    std::string source = "fn add(x: i32, y: i32) -> i32 { ret x + y; } fn "
                         "main() -> i32 { ret add(2, 3); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *addFn = dynamic_cast<FnStatement *>(ast[0]);
    auto *mainFn = dynamic_cast<FnStatement *>(ast[1]);
    ASSERT_NE(addFn, nullptr, "Expected function 'add'");
    ASSERT_NE(mainFn, nullptr, "Expected function 'main'");
    ASSERT_EQ(addFn->GetReturnDatatype()->ToString(), "i32",
              "Function add return type should be i32");
    ASSERT_EQ(mainFn->GetReturnDatatype()->ToString(), "i32",
              "Function main return type should be i32");
}

// Test 14: String literal.
TEST(SemanticStringLiteralTest)
{
    std::string source = "fn main() -> readonly i8* { ret \"hello\"; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "readonly i8*",
              "Return type should be readonly i8*");
}

// Test 15: Character literal.
TEST(SemanticCharLiteralTest)
{
    std::string source = "fn main() -> i8 { ret 'a'; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i8",
              "Return type should be i8");
}

// Test 16: Mixed arithmetic with implicit cast.
TEST(SemanticMixedArithmeticTest)
{
    std::string source = "fn main() -> f32 { ret 3 as f32 + 2.0; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "f32",
              "Return type should be f32");
}

// Test 17: Comparison after arithmetic.
TEST(SemanticComparisonArithmeticTest)
{
    std::string source = "fn main() -> bool { ret (5 + 3) == 8; }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "bool",
              "Return type should be bool");
}

// Test 18: Complex arithmetic expression.
TEST(SemanticComplexArithmeticTest)
{
    std::string source = "fn main() -> i32 { ret ((1 + 2) * 3) - (4 / 2); }";
    Lexer lexer;
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    SemanticAnalyzer analyzer(ast);
    analyzer.Analize();
    auto *fn = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fn, nullptr, "Expected a function declaration");
    ASSERT_EQ(fn->GetReturnDatatype()->ToString(), "i32",
              "Return type should be i32");
} // Test 6: Function Analysis with Unary Postfix ++ and --
// Input: functions that use postfix increment and decrement operators.
TEST(SemanticAnalyzerUnaryPostfixTest)
{
    std::string source = "fn inc(x: i32) -> i32 { x++; ret x; }"
                         "fn dec(x: i32) -> i32 { x--; ret x; }";
    Lexer lexer;
    lexer.Tokenize(source, "unary_postfix_fn.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 2,
              "Expected two top–level statements for unary postfix functions");

    // Check the 'inc' function.
    auto *fnStmtInc = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmtInc, nullptr, "fnStmt should be a FnStatement");

    bool foundPostfixInc = false;
    for (auto stmt : fnStmtInc->GetBlockStatement()->GetStatements())
    {
        auto *exprStmt = dynamic_cast<ExpressionStatement *>(stmt);
        if (exprStmt)
        {
            auto *unaryExpr =
                dynamic_cast<UnaryExpression *>(exprStmt->GetExpression());
            if (unaryExpr && unaryExpr->IsPostfix())
            {
                foundPostfixInc = true;
                break;
            }
        }
    }
    ASSERT_EQ(foundPostfixInc, true,
              "Function 'inc' should contain a unary postfix expression");

    auto *fnStmtDec = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmtInc, nullptr, "fnStmt should be a FnStatement");

    bool foundPostfixDec = false;
    for (auto stmt : fnStmtDec->GetBlockStatement()->GetStatements())
    {
        auto *exprStmt = dynamic_cast<ExpressionStatement *>(stmt);
        if (exprStmt)
        {
            auto *unaryExpr =
                dynamic_cast<UnaryExpression *>(exprStmt->GetExpression());
            if (unaryExpr && unaryExpr->IsPostfix())
            {
                foundPostfixDec = true;
                break;
            }
        }
    }
    ASSERT_EQ(foundPostfixDec, true,
              "Function 'dec' should contain a unary postfix expression");
}

TEST(SemanticAnalyzerUnaryRef)
{
    std::string source = "fn main() {  a: i32 = 1; ptr: i32* = &a;  }";
    Lexer lexer;
    lexer.Tokenize(source, "unary_ref.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerUnaryDeref)
{
    std::string source =
        "fn main() {  a: i32 = 1; ptr: i32* = &a; *ptr = 12;  }";
    Lexer lexer;
    lexer.Tokenize(source, "unary_ref.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}

TEST(SemanticAnalyzerWhileLoop)
{
    std::string source = R"(
        fn main()
        {
            a: i32 = 0;
            while a < 5
            {
                a++;
            }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_ref.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerWhileLoopBreak)
{
    std::string source = R"(
        fn main()
        {
            a: i32 = 0;
            while a < 5
            {
                for i : i32 = 0; i < 4; i++
                {
                    if i > 3
                    {
                        break;
                    }
                }
                if a == 3
                {
                    break;
                }
                a++;
            }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_ref.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerWhileLoopContinue)
{
    std::string source = R"(
        fn main()
        {
            a: i32 = 0;
            while a < 5
            {
                for i : i32 = 0; i < 4; i++
                {
                    if i > 3
                    {
                        continue;
                    }
                }
                if a == 3
                {
                    continue;
                }
                a++;
            }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "unary_ref.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerArrayVarDecl)
{
    std::string source = R"(
        fn main()
        {
            arr: i32[2];
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerArrayVarDeclInitList)
{
    std::string source = R"(
        fn main()
        {
            arr: u32[3] = {1,  2, 3};
            arr [1] = 3;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "array_decl.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerFunctionPrototype)
{
    Lexer lexer;
    std::string source =
        "fn printf(str: readonly i8*) -> i32; fn main() { printf(\"Hi\"); }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
TEST(SemanticAnalyzerFunctionPrototypeVarArgs)
{
    Lexer lexer;
    std::string source = "fn printf(str: readonly i8*, ...) -> i32; fn main() "
                         "{ printf(\"Hi\", 12); }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
