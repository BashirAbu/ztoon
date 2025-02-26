#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

TEST(SemanticTesting)
{
    std::string source =
        R"(pi: f32 = 3.14;
        r: i32 = 7.0 as i32;
        r *= r;
        area: f32 = pi * r as f32;
        a: u8 = 22;
        f: f64 = .324343434;
        big: u64 = (f as i32 * 12) as u64 - ( 5 < 2 || 1> 0  + 1.0 as i32 ) as u64;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "semantics.ztoon");
    Parser parser(lexer.GetTokens());
    auto statements = parser.Parse();
    SemanticAnalyzer analyzer(statements);
    analyzer.Analize();
}

TEST(SemanticAnalyzerIfStatement)
{
    std::string source = R"(
        if true {
            b: i32 = 12;
        }
        else if 5 > 2 && 11 != 1 || true {
            c: f64 = 3.14;
        }
        
        else {
        {}{}{}
            d: u8 = 1;
        }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "ifStatement.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
}

TEST(SemanticAnalyzerScope)
{

    std::string source = R"(
        a : i32 = 1;
        {
            a: f32 = 3243.0;
            b: i32 = a as i32;
            {
            b = 0;
                {
                b = 3;
                    {
                        man: i32 = b;
                    }
                  b = 1;  
                }
                b = 0;
            }
            
        }
        b: i32 = 333;
        c: i32 = b;
    )";

    Lexer lexer;
    lexer.Tokenize(source, "scopes.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
}

TEST(SemanticAnalyzerTernaryExpression)
{

    std::string source = R"(
       cond: bool = true;
       a: i32 = cond? (12 * 33): (1.0 * 3.0) as i32;
    )";

    Lexer lexer;
    lexer.Tokenize(source, "scopes.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
}

TEST(SemanticAnalyzerWhileLoopStatement)
{
    std::string source = R"(
       while 1 + 2 == 3
       {
           a: i32 = 12;
       }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "scopes.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
}

TEST(SemanticAnalyzerForLoopStatement)
{
    std::string source = R"(
       for ; ;
       {
           ;
       }
    )";

    Lexer lexer;
    lexer.Tokenize(source, "scopes.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
}

//--------------------------------------------------------------------------
// Test 1: Valid While Loop
// Source: "x: i32 = 0; while (x < 10) { x = x + 1; }"
// The semantic analyzer should accept this without errors and build a
// WhileLoopStatement.
TEST(SemanticWhileLoopValidTest)
{
    std::string source = "x: i32 = 0; while (x < 10) { x = x + 1; }";
    Lexer lexer;
    lexer.Tokenize(source, "while_valid.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();

    // Expect two top-level statements: a var-declaration for x and the while
    // loop.
    ASSERT_EQ(stmts.size(), 2,
              "Expected 2 top-level statements (var decl and while loop)");

    auto *whileStmt = dynamic_cast<WhileLoopStatement *>(stmts[1]);
    ASSERT_NE(whileStmt, nullptr,
              "Second statement should be a WhileLoopStatement");

    // The condition of the while loop should be a binary expression (x < 10)
    ASSERT_NE(whileStmt->GetCondition(), nullptr,
              "While loop must have a condition");

    // The block must be a BlockStatement containing one assignment statement.
    auto *block =
        dynamic_cast<BlockStatement *>(whileStmt->GetBlockStatement());
    ASSERT_NE(block, nullptr, "While loop block must be a BlockStatement");
    ASSERT_EQ(block->GetStatements().size(), 1,
              "While loop block should contain one statement");
}

//--------------------------------------------------------------------------
// Test 2: Valid For Loop
// Source: "x: i32 = 0; for (i: i32 = 0; i < 5; i = i + 1) { x = x + i; }"
// The analyzer should correctly process the for-loop parts (init, condition,
// update, and block).
TEST(SemanticForLoopValidTest)
{
    std::string source =
        "x: i32 = 0; for (i: i32 = 0; i < 5; i = i + 1) { x = x + i; }";
    Lexer lexer;
    lexer.Tokenize(source, "for_valid.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();

    // Expect two top-level statements: one for x declaration and one for the
    // for loop (which may be wrapped in a block).
    ASSERT_EQ(stmts.size(), 2,
              "Expected 2 top-level statements (var decl and for loop)");

    // The for loop is expected to be wrapped in a BlockStatement.
    auto *forBlock = dynamic_cast<BlockStatement *>(stmts[1]);
    ASSERT_NE(forBlock, nullptr,
              "The for loop should be wrapped in a BlockStatement");
    ASSERT_EQ(forBlock->GetStatements().size(), 1,
              "For loop block should contain one statement");

    auto *forStmt =
        dynamic_cast<ForLoopStatement *>(forBlock->GetStatements()[0]);
    ASSERT_NE(forStmt, nullptr, "Expected a ForLoopStatement");

    // Check that the for loop has a non-null init, condition, update, and
    // block.
    ASSERT_NE(forStmt->GetInit(), nullptr, "For loop init must be present");
    ASSERT_NE(forStmt->GetCondition(), nullptr,
              "For loop condition must be present");
    // Update might be optional; if provided, it must be valid.
    ASSERT_NE(forStmt->GetBlockStatement(), nullptr,
              "For loop block must be present");
}

//--------------------------------------------------------------------------
// Test 3: For Loop with Empty Init/Update Parts
// Source: "for (; x < 10; ) { x = x + 1; }"
// The analyzer should accept loops with omitted init and update.
TEST(SemanticForLoopEmptyPartsTest)
{
    std::string source = "x: i32 = 0; for (; x < 10; ) { x = x + 1; }";
    Lexer lexer;
    lexer.Tokenize(source, "for_empty.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();

    // Expect a single top-level statement wrapping the for loop.
    ASSERT_EQ(stmts.size(), 2,
              "Expected 2 top-level statement for for loop with empty parts");

    auto *forStmt = dynamic_cast<ForLoopStatement *>(
        dynamic_cast<BlockStatement *>(stmts[1])->GetStatements()[0]);
    ASSERT_NE(forStmt, nullptr, "Expected a ForLoopStatement");
    // For this test, init and update may be null; condition and block must be
    // present.
    ASSERT_NE(forStmt->GetCondition(), nullptr,
              "For loop condition must be present");
    ASSERT_NE(forStmt->GetBlockStatement(), nullptr,
              "For loop block must be present");
}

//--------------------------------------------------------------------------
// Test 4: Valid Unary Postfix Increment (++)
// Source: "x: i32 = 5; y: i32 = x++;"
// Semantic analysis should transform the postfix ++ on x into an additional
// assignment statement.
TEST(SemanticPostfixIncrementValidTest)
{
    std::string source = "x: i32 = 5; y: i32 = x++;";
    Lexer lexer;
    lexer.Tokenize(source, "postfix_inc_valid.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();

    // The semantic analyzer should insert an extra assignment statement after
    // processing x++. With two declarations in the source, we expect a total of
    // 3 top-level statements.
    ASSERT_EQ(
        stmts.size(), 3,
        "Expected 3 top-level statements after postfix ++ transformation");

    // The extra statement should be a VarAssignmentStatement (generated from
    // x++).
    auto *assignStmt = dynamic_cast<VarAssignmentStatement *>(stmts[2]);
    ASSERT_NE(assignStmt, nullptr,
              "Expected an extra VarAssignmentStatement for postfix ++");
}

//--------------------------------------------------------------------------
// Test 5: Valid Unary Postfix Decrement (--)
// Source: "x: i32 = 10; y: i32 = x--;"
// Semantic analysis should transform the postfix -- on x.
TEST(SemanticPostfixDecrementValidTest)
{
    std::string source = "x: i32 = 10; y: i32 = x--;";
    Lexer lexer;
    lexer.Tokenize(source, "postfix_dec_valid.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();

    // Expect an extra assignment statement for the postfix decrement.
    ASSERT_EQ(
        stmts.size(), 3,
        "Expected 3 top-level statements after postfix -- transformation");
    auto *assignStmt = dynamic_cast<VarAssignmentStatement *>(stmts[2]);
    ASSERT_NE(assignStmt, nullptr,
              "Expected an extra VarAssignmentStatement for postfix --");
}
