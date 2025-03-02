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

TEST(SemanticAnalyzerRecursiveFunction)
{
    std::string source = R"(
        fn add () -> i32
        {
            a : i32 = add();
            ret 2;
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "postfix_dec_valid.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    SemanticAnalyzer analyzer(stmts);
    analyzer.Analize();
}
// Test 1: Function Prototype Analysis
// Input: a function prototype with no parameters.
TEST(SemanticAnalyzerFunctionPrototypeTest)
{
    std::string source = "fn proto() -> i32;";
    Lexer lexer;
    lexer.Tokenize(source, "func_proto.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    // Expect one top–level statement.
    ASSERT_EQ(stmts.size(), 1,
              "Expected one top–level statement for a function prototype");

    auto *fnStmt = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmt, nullptr, "fnStmt should be a FnStatement");

    // A prototype should have no parameters, a return type of "i32", and be
    // marked as a prototype.
    ASSERT_EQ(fnStmt->GetParameters().size(), 0,
              "Function prototype should have zero parameters");
    ASSERT_NE(fnStmt->GetReturnDatatype(), nullptr,
              "Function prototype should have a return type");
    ASSERT_EQ(fnStmt->GetReturnDatatype()->GetLexeme(), std::string("i32"),
              "Return type should be 'i32'");
    ASSERT_EQ(fnStmt->IsPrototype(), true,
              "FnExpression should be marked as a prototype");
}

// Test 2: Function Definition Analysis
// Input: a function with parameters, a return type and a block containing a
// return statement.
TEST(SemanticAnalyzerFunctionDefinitionTest)
{
    std::string source = "fn add(a: i32, b: i32) -> i32 { ret a + b; }";
    Lexer lexer;
    lexer.Tokenize(source, "func_def.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    ASSERT_EQ(stmts.size(), 1,
              "Expected one top–level statement for a function definition");
    auto *fnStmt = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmt, nullptr, "fnStmt should be a FnStatement");

    // This is a full definition so it should NOT be marked as a prototype.
    ASSERT_EQ(fnStmt->IsPrototype(), false,
              "Function definition should not be a prototype");
    ASSERT_EQ(fnStmt->GetParameters().size(), 2,
              "Function 'add' should have 2 parameters");

    // Verify parameter names and types.
    auto *paramA = fnStmt->GetParameters()[0];
    ASSERT_EQ(paramA->GetIdentifier()->GetLexeme(), std::string("a"),
              "First parameter should be 'a'");
    ASSERT_EQ(paramA->GetDataType()->GetLexeme(), std::string("i32"),
              "Type of 'a' should be 'i32'");
    auto *paramB = fnStmt->GetParameters()[1];
    ASSERT_EQ(paramB->GetIdentifier()->GetLexeme(), std::string("b"),
              "Second parameter should be 'b'");
    ASSERT_EQ(paramB->GetDataType()->GetLexeme(), std::string("i32"),
              "Type of 'b' should be 'i32'");

    // Check that a block statement exists and contains a return statement.
    ASSERT_NE(fnStmt->GetBlockStatement(), nullptr,
              "Function definition must have a block statement");
    bool foundRet = false;
    for (auto stmt : fnStmt->GetBlockStatement()->GetStatements())
    {
        if (dynamic_cast<RetStatement *>(stmt))
        {
            foundRet = true;
            break;
        }
    }
    ASSERT_EQ(foundRet, true,
              "Function block should contain a return statement");
}

// Test 3: Function Call Analysis
// Input: a function definition and a variable assignment that calls it.
TEST(SemanticAnalyzerFunctionCallTest)
{
    std::string source =
        "fn square(x: i32) -> i32 { ret x * x; } x: i32 = square(5);";
    Lexer lexer;
    lexer.Tokenize(source, "func_call.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    // Expect two top–level statements: function definition and a variable
    // assignment.
    ASSERT_EQ(stmts.size(), 2,
              "Expected two top–level statements for function call test");

    // Check the function definition.
    auto *fnStmt = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmt, nullptr, "fnStmt should be a FnStatement");

    // Check the variable assignment; its expression should be a function call.
    auto *varAssign = dynamic_cast<VarDeclStatement *>(stmts[1]);
    ASSERT_NE(varAssign, nullptr,
              "Second statement should be a VarAssignmentStatement");
    auto *fnCall = dynamic_cast<FnCallExpression *>(varAssign->GetExpression());
    ASSERT_NE(fnCall, nullptr,
              "Variable assignment expression should be a FnCallExpression");
    ASSERT_EQ(fnCall->GetIdentifier()->GetLexeme(), std::string("square"),
              "Function call should be to 'square'");
    ASSERT_EQ(fnCall->GetArgs().size(), 1,
              "Function call should have one argument");

    auto *arg = dynamic_cast<PrimaryExpression *>(fnCall->GetArgs()[0]);
    ASSERT_NE(arg, nullptr, "Argument should be a PrimaryExpression");
    ASSERT_EQ(arg->GetPrimary()->GetLexeme(), std::string("5"),
              "Argument should be '5'");
}

// Test 4: Recursive Function Analysis
// Input: a recursive function and a function call.
TEST(SemanticAnalyzerRecursiveFunctionTest)
{
    std::string source = "fn fact(n: i32) -> i32 { if n <= 1 { ret 1; } else { "
                         "ret n * fact(n - 1); } }"
                         " x: i32 = fact(5);";
    Lexer lexer;
    lexer.Tokenize(source, "recursive_fn.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    ASSERT_EQ(stmts.size(), 2,
              "Expected two top–level statements for recursive function test");
    auto *fnStmt = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmt, nullptr, "fnStmt should be a FnStatement");

    ASSERT_EQ(fnStmt->GetIdentifier()->GetLexeme(), std::string("fact"),
              "Function name should be 'fact'");

    // Check that the function has one parameter.
    ASSERT_EQ(fnStmt->GetParameters().size(), 1,
              "Function 'fact' should have 1 parameter");

    // Check the variable assignment calling 'fact'.
    auto *varAssign = dynamic_cast<VarDeclStatement *>(stmts[1]);
    ASSERT_NE(varAssign, nullptr,
              "Second statement should be a VarAssignmentStatement");
    auto *fnCall = dynamic_cast<FnCallExpression *>(varAssign->GetExpression());
    ASSERT_NE(fnCall, nullptr, "Function call should be a FnCallExpression");
    ASSERT_EQ(fnCall->GetIdentifier()->GetLexeme(), std::string("fact"),
              "Function call should be to 'fact'");
}

// Test 5: Function with While Loop Analysis
// Input: a function containing a while loop and a return statement.
TEST(SemanticAnalyzerWhileLoopFunctionTest)
{
    std::string source = "fn loopFunc(n: i32) -> i32 { "
                         "   while n > 0 { n = n - 1; } "
                         "   ret n; "
                         "}";
    Lexer lexer;
    lexer.Tokenize(source, "while_fn.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1,
              "Expected one top–level statement for while loop function");
    auto *fnStmt = dynamic_cast<FnStatement *>(stmts[0]);
    ASSERT_NE(fnStmt, nullptr, "fnStmt should be a FnStatement");

    // Check that the function block contains a while loop.
    bool foundWhile = false;
    for (auto stmt : fnStmt->GetBlockStatement()->GetStatements())
    {
        if (dynamic_cast<WhileLoopStatement *>(stmt))
        {
            foundWhile = true;
            break;
        }
    }
    ASSERT_EQ(foundWhile, true,
              "Function block should contain a while loop statement");
}

// Test 6: Function Analysis with Unary Postfix ++ and --
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
