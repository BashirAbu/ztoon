#include "parser/parser.h"
#include "ztest.h"
#include <string>

// --- Global Scope Tests ---

// Test 1: Valid global function declaration.
TEST(ParserGlobalFunctionDeclarationTest)
{
    // Global scope should allow a function declaration.
    Lexer lexer;
    std::string source = "fn main() {}";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    // Expect one declaration.
    ASSERT_EQ(ast.size(), 1, "Global scope should contain one declaration");
    // At global scope the node must be either a FnStatement or
    // VarDeclStatement.
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
    // In this case, it must be a function declaration.
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration must be a function declaration");
    ASSERT_EQ(varDecl, nullptr,
              "Global declaration must not be a variable declaration here");
    ASSERT_EQ(fnStmt->GetIdentifier()->GetLexeme(), "main",
              "Function name should be 'main'");
}

// Test 2: Valid global variable declaration.
TEST(ParserGlobalVarDeclarationTest)
{
    // Global scope should allow a variable declaration.
    Lexer lexer;
    std::string source = "x: i32 = 42;";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    ASSERT_EQ(ast.size(), 1, "Global scope should contain one declaration");
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
    // For a variable declaration, the FnStatement cast must be null and
    // VarDeclStatement non-null.
    ASSERT_EQ(fnStmt, nullptr,
              "Global declaration should not be a function declaration");
    ASSERT_NE(varDecl, nullptr,
              "Global declaration must be a variable declaration");
    ASSERT_EQ(varDecl->GetIdentifier()->GetLexeme(), "x",
              "Variable name should be 'x'");
    ASSERT_EQ(varDecl->GetDataType()->ToString(), "i32",
              "Variable type should be 'i32'");
}

// Test 3: Global scope violation – expression statement.
// TEST(ParserGlobalExpressionViolationTest)
// {
//     // Global scope must not allow an expression statement.
//     Lexer lexer;
//     std::string source = "42 + 2;";
//     lexer.Tokenize(source, "test.ztoon");
//     Parser parser(lexer.GetTokens());
//     auto ast = parser.Parse();
//     // Here the AST node is not a function declaration nor a variable
//     // declaration.
//     auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
//     auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
//     ASSERT_EQ(fnStmt, nullptr,
//               "Global expression should not be a function declaration");
//     ASSERT_EQ(varDecl, nullptr,
//               "Global expression should not be a variable declaration");
// }

// // Test 4: Global scope violation – if statement.
// TEST(ParserGlobalIfViolationTest)
// {
//     // Global scope must not allow an if statement.
//     Lexer lexer;
//     std::string source = "if true {}";
//     lexer.Tokenize(source, "test.ztoon");
//     Parser parser(lexer.GetTokens());
//     auto ast = parser.Parse();
//     auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
//     auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
//     ASSERT_EQ(fnStmt, nullptr,
//               "Global if statement should not be a function declaration");
//     ASSERT_EQ(varDecl, nullptr,
//               "Global if statement should not be a variable declaration");
// }

// Test 5: Global scope violation – while loop.
// TEST(ParserGlobalWhileViolationTest)
// {
//     // Global scope must not allow a while loop.
//     Lexer lexer;
//     std::string source = "while x {}";
//     lexer.Tokenize(source, "test.ztoon");
//     Parser parser(lexer.GetTokens());
//     auto ast = parser.Parse();
//     auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
//     auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
//     ASSERT_EQ(fnStmt, nullptr,
//               "Global while loop should not be a function declaration");
//     ASSERT_EQ(varDecl, nullptr,
//               "Global while loop should not be a variable declaration");
// }

// Test 6: Global scope violation – for loop.
// TEST(ParserGlobalForViolationTest)
// {
//     // Global scope must not allow a for loop.
//     Lexer lexer;
//     std::string source = "for(x: i32 = 0; x < 10; x = x + 1) {}";
//     lexer.Tokenize(source, "test.ztoon");
//     Parser parser(lexer.GetTokens());
//     auto ast = parser.Parse();
//     auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
//     auto *varDecl = dynamic_cast<VarDeclStatement *>(ast[0]);
//     ASSERT_EQ(fnStmt, nullptr,
//               "Global for loop should not be a function declaration");
//     ASSERT_EQ(varDecl, nullptr,
//               "Global for loop should not be a variable declaration");
// }

// --- Valid Statements Inside a Function Block ---

// Test 7: Valid if statement inside a function block.
TEST(ParserIfInsideFunctionTest)
{
    // Inside a function, an if statement is allowed.
    Lexer lexer;
    std::string source = "fn main() { if true {} }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    // Global AST should contain one function declaration.
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    // Inside the function block, the first statement should be an if statement.
    auto *ifStmt = dynamic_cast<IfStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(ifStmt, nullptr,
              "Statement inside the function block should be an if statement");
    std::string cond = ifStmt->GetExpression()->GetCodeErrString().str;
    ASSERT_NE(cond.find("true"), std::string::npos,
              "If condition should contain 'true'");
}

// Test 8: Valid while loop inside a function block.
TEST(ParserWhileInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { while x {} }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *whileStmt = dynamic_cast<WhileLoopStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(whileStmt, nullptr,
              "Statement inside the function block should be a while loop");
    std::string cond = whileStmt->GetCondition()->GetCodeErrString().str;
    ASSERT_NE(cond.find("x"), std::string::npos,
              "While loop condition should contain 'x'");
}

// Test 10: Valid variable assignment inside a function block.
TEST(ParserAssignmentInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { x = 100; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *assignStmt = dynamic_cast<VarAssignmentStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        assignStmt, nullptr,
        "Statement inside function block should be a variable assignment");
    ASSERT_EQ(assignStmt->GetLValue()->GetCodeErrString().str, "x",
              "Assigned variable should be 'x'");
    std::string expr = assignStmt->GetRValue()->GetCodeErrString().str;
    ASSERT_NE(expr.find("100"), std::string::npos,
              "Assignment expression should contain '100'");
}

// Test 11: Valid compound assignment inside a function block.
TEST(ParserCompoundAssignmentInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { x += 1; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *compoundStmt = dynamic_cast<VarCompoundAssignmentStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        compoundStmt, nullptr,
        "Statement inside function block should be a compound assignment");
    ASSERT_EQ(compoundStmt->GetLValue()->GetCodeErrString().str, "x",
              "Compound assignment variable should be 'x'");
    auto *binaryExpr =
        dynamic_cast<BinaryExpression *>(compoundStmt->GetRValue());
    ASSERT_NE(binaryExpr, nullptr,
              "Compound assignment should yield a binary expression");
    ASSERT_EQ(binaryExpr->GetOperator()->GetType(), TokenType::PLUS,
              "Compound assignment should translate to '+' operator");
}

// Test 12: Valid expression statement inside a function block.
TEST(ParserExpressionInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { 42 + 2; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        exprStmt, nullptr,
        "Statement inside function block should be an expression statement");
    auto *binaryExpr =
        dynamic_cast<BinaryExpression *>(exprStmt->GetExpression());
    ASSERT_NE(binaryExpr, nullptr, "Expression should be a binary expression");
    std::string codeStr = binaryExpr->GetCodeErrString().str;
    ASSERT_NE(codeStr.find("42"), std::string::npos,
              "Binary expression should contain '42'");
    ASSERT_NE(codeStr.find("2"), std::string::npos,
              "Binary expression should contain '2'");
}

// Test 13: Valid function call inside a function block.
TEST(ParserFunctionCallInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { foo(1, 2); }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        exprStmt, nullptr,
        "Statement inside function block should be an expression statement");
    auto *fnCall = dynamic_cast<FnCallExpression *>(exprStmt->GetExpression());
    ASSERT_NE(fnCall, nullptr, "Expression should be a function call");
    ASSERT_EQ(fnCall->GetIdentifier()->GetLexeme(), "foo",
              "Function call should be to 'foo'");
    ASSERT_EQ(fnCall->GetArgs().size(), 2,
              "Function call should have 2 arguments");
}

// Test 14: Valid unary expressions inside a function block.
TEST(ParserUnaryExpressionInsideFunctionTest)
{
    {
        Lexer lexer;
        std::string source = "fn main() { -x; }";
        lexer.Tokenize(source, "test.ztoon");
        Parser parser(lexer.GetTokens());
        auto ast = parser.Parse();
        auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
        ASSERT_NE(fnStmt, nullptr,
                  "Global declaration should be a function declaration");
        auto *exprStmt = dynamic_cast<ExpressionStatement *>(
            fnStmt->GetBlockStatement()->GetStatements()[0]);
        ASSERT_NE(exprStmt, nullptr,
                  "Statement should be an expression statement inside function "
                  "block");
        auto *unaryExpr =
            dynamic_cast<UnaryExpression *>(exprStmt->GetExpression());
        ASSERT_NE(unaryExpr, nullptr,
                  "Expression should be a unary expression");
        ASSERT_EQ(unaryExpr->GetOperator()->GetLexeme(), "-",
                  "Unary operator should be '-'");
    }
    {
        Lexer lexer;
        std::string source = "fn main() { x++; }";
        lexer.Tokenize(source, "test.ztoon");
        Parser parser(lexer.GetTokens());
        auto ast = parser.Parse();
        auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
        ASSERT_NE(fnStmt, nullptr,
                  "Global declaration should be a function declaration");
        auto *exprStmt = dynamic_cast<ExpressionStatement *>(
            fnStmt->GetBlockStatement()->GetStatements()[0]);
        ASSERT_NE(exprStmt, nullptr,
                  "Statement should be an expression statement inside function "
                  "block");
        auto *unaryExpr =
            dynamic_cast<UnaryExpression *>(exprStmt->GetExpression());
        ASSERT_NE(unaryExpr, nullptr,
                  "Expression should be a unary expression");
        ASSERT_EQ(unaryExpr->IsPostfix(), true,
                  "Unary expression should be postfix");
    }
}

// Test 15: Valid grouping expression inside a function block.
TEST(ParserGroupingExpressionInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { (x); }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        exprStmt, nullptr,
        "Statement should be an expression statement inside function block");
    auto *groupExpr =
        dynamic_cast<GroupingExpression *>(exprStmt->GetExpression());
    ASSERT_NE(groupExpr, nullptr, "Expression should be a grouping expression");
}

// Test 16: Valid cast expression inside a function block.
TEST(ParserCastExpressionInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { x as i32; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        exprStmt, nullptr,
        "Statement should be an expression statement inside function block");
    auto *castExpr = dynamic_cast<CastExpression *>(exprStmt->GetExpression());
    ASSERT_NE(castExpr, nullptr, "Expression should be a cast expression");
    ASSERT_EQ(castExpr->GetCastToType()->ToString(), "i32",
              "Cast target type should be 'i32'");
}

// Test 17: Valid ternary expression inside a function block.
TEST(ParserTernaryExpressionInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { x ? y : z; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        exprStmt, nullptr,
        "Statement should be an expression statement inside function block");
    auto *ternaryExpr =
        dynamic_cast<TernaryExpression *>(exprStmt->GetExpression());
    ASSERT_NE(ternaryExpr, nullptr,
              "Expression should be a ternary expression");
    ASSERT_NE(ternaryExpr->GetCondition(), nullptr,
              "Ternary condition should not be null");
    ASSERT_NE(ternaryExpr->GetTrueExpression(), nullptr,
              "Ternary true expression should not be null");
    ASSERT_NE(ternaryExpr->GetFalseExpression(), nullptr,
              "Ternary false expression should not be null");
}

// Test 18: Valid readonly data type declaration inside a function block.
TEST(ParserReadonlyDataTypeInsideFunctionTest)
{
    Lexer lexer;
    std::string source = "fn main() { x: readonly i32; }";
    lexer.Tokenize(source, "test.ztoon");
    Parser parser(lexer.GetTokens());
    auto ast = parser.Parse();
    auto *fnStmt = dynamic_cast<FnStatement *>(ast[0]);
    ASSERT_NE(fnStmt, nullptr,
              "Global declaration should be a function declaration");
    auto *varDecl = dynamic_cast<VarDeclStatement *>(
        fnStmt->GetBlockStatement()->GetStatements()[0]);
    ASSERT_NE(
        varDecl, nullptr,
        "Statement inside function block should be a variable declaration");
    std::string dtStr = varDecl->GetDataType()->ToString();
    ASSERT_NE(dtStr.find("readonly"), std::string::npos,
              "Data type string should contain 'readonly'");
}
