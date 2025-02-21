#include "lexer/lexer.h"
#include "parser/parser.h"
#include "ztest.h"
#include <string>

TEST(ParserNewFeatureTest)
{
    std::string source = "a *= (1 | 2 * 4);";
    Lexer lexer;
    lexer.Tokenize(source, "assignment.ztoon");
    Parser parser(lexer.GetTokens());
    parser.Parse();
    parser.PrettyPrintAST();
}

//--------------------------------------------------------------------------
// Test: Unary Minus Expression
// Input: "-5;"
TEST(ParserUnaryMinusTest)
{
    std::string source = "-5;";
    std::string filename = "test_unary_minus.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Statement should be an ExpressionStatement");

    // The expression should be a UnaryExpression with '-' operator.
    UnaryExpression const *unaryExpr =
        dynamic_cast<UnaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(unaryExpr, nullptr, "Expression should be a UnaryExpression");
    ASSERT_EQ(unaryExpr->GetOperator()->GetType(), TokenType::DASH,
              "Operator should be DASH");

    // The operand of the unary operator should be the literal "5".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(
            unaryExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Unary right expression should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "5",
              "Literal should be '5'");
}

//--------------------------------------------------------------------------
// Test: Unary Plus-Plus Expression
// Input: "++5;"
TEST(ParserUnaryPlusPlusTest)
{
    std::string source = "++5;";
    std::string filename = "test_unary_plus_plus.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Statement should be an ExpressionStatement");

    // The expression should be a UnaryExpression with '++' operator.
    UnaryExpression const *unaryExpr =
        dynamic_cast<UnaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(unaryExpr, nullptr, "Expression should be a UnaryExpression");
    ASSERT_EQ(unaryExpr->GetOperator()->GetType(), TokenType::PLUS_PLUS,
              "Operator should be PLUS_PLUS");

    // The operand should be the literal "5".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(
            unaryExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Unary right expression should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "5",
              "Literal should be '5'");
}

//--------------------------------------------------------------------------
// Test: Unary Exclamation Expression
// Input: "!true;"
TEST(ParserUnaryExclamationTest)
{
    std::string source = "!true;";
    std::string filename = "test_unary_exclamation.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Statement should be an ExpressionStatement");

    // The expression should be a UnaryExpression with '!' operator.
    UnaryExpression const *unaryExpr =
        dynamic_cast<UnaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(unaryExpr, nullptr, "Expression should be a UnaryExpression");
    ASSERT_EQ(unaryExpr->GetOperator()->GetType(), TokenType::EXCLAMATION,
              "Operator should be EXCLAMATION");

    // The operand should be the literal "true".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(
            unaryExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Unary right expression should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "true",
              "Literal should be 'true'");
}

//--------------------------------------------------------------------------
// Test: sizeof Expression
// Input: "sizeof(1);"
TEST(ParserSizeofTest)
{
    std::string source = "sizeof(1);";
    std::string filename = "test_sizeof.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Statement should be an ExpressionStatement");

    // The expression should be a UnaryExpression with 'sizeof' operator.
    UnaryExpression const *unaryExpr =
        dynamic_cast<UnaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(unaryExpr, nullptr, "Expression should be a UnaryExpression");
    ASSERT_EQ(unaryExpr->GetOperator()->GetType(), TokenType::SIZEOF,
              "Operator should be SIZEOF");

    // The operand inside sizeof should be the literal "1".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(
            unaryExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Sizeof's right expression should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "1",
              "Literal should be '1'");
}

//--------------------------------------------------------------------------
// Test: Combined Unary with Binary Expression
// Input: "1 + -2;"
TEST(ParserCombinedUnaryBinaryTest)
{
    std::string source = "1 + -2;";
    std::string filename = "test_combined_unary_binary.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Statement should be an ExpressionStatement");

    // The top-level expression should be a BinaryExpression for addition.
    BinaryExpression const *binaryExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binaryExpr, nullptr,
              "Top-level expression should be a BinaryExpression");
    ASSERT_EQ(binaryExpr->GetOperator()->GetLexeme(), "+",
              "Operator should be '+'");

    // Left operand should be the literal "1".
    PrimaryExpression const *leftPrim = dynamic_cast<PrimaryExpression const *>(
        binaryExpr->GetLeftExpression());
    ASSERT_NE(leftPrim, nullptr, "Left operand should be a PrimaryExpression");
    ASSERT_EQ(leftPrim->GetPrimary()->GetLexeme(), "1",
              "Left literal should be '1'");

    // Right operand should be a UnaryExpression with '-' operator.
    UnaryExpression const *unaryExpr =
        dynamic_cast<UnaryExpression const *>(binaryExpr->GetRightExpression());
    ASSERT_NE(unaryExpr, nullptr, "Right operand should be a UnaryExpression");
    ASSERT_EQ(unaryExpr->GetOperator()->GetType(), TokenType::DASH,
              "Unary operator should be DASH");

    // The operand of the unary operator should be the literal "2".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(
            unaryExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Unary's right operand should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "2",
              "Right literal should be '2'");
}

//--------------------------------------------------------------------------
// Test 1: Variable Declaration Statement
// "a: i32 = 5;"
TEST(ParserVarDeclTest)
{
    std::string source = "a: i32 = 5;";
    std::string filename = "test_var_decl.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    // The statement should be a variable declaration.
    VarDeclStatement *varDecl = dynamic_cast<VarDeclStatement *>(stmts[0]);
    ASSERT_NE(varDecl, nullptr, "Parsed statement should be VarDeclStatement");

    // Check the identifier and datatype tokens.
    ASSERT_EQ(varDecl->GetIdentifier()->GetLexeme(), "a",
              "Identifier should be 'a'");
    ASSERT_EQ(varDecl->GetDataType()->GetLexeme(), "i32",
              "Data type should be 'i32'");

    // The expression should be a primary expression representing the literal
    // "5".
    PrimaryExpression const *primExpr =
        dynamic_cast<PrimaryExpression const *>(varDecl->GetExpression());
    ASSERT_NE(primExpr, nullptr, "Expression should be a PrimaryExpression");
    ASSERT_EQ(primExpr->GetPrimary()->GetLexeme(), "5",
              "Literal should be '5'");
}

//--------------------------------------------------------------------------
// Test 2: Variable Assignment Statement
// "a = 10;"
TEST(ParserVarAssignmentTest)
{
    std::string source = "a = 10;";
    std::string filename = "test_var_assign.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    // The statement should be a variable assignment.
    VarAssignmentStatement *varAssign =
        dynamic_cast<VarAssignmentStatement *>(stmts[0]);
    ASSERT_NE(varAssign, nullptr,
              "Parsed statement should be VarAssignmentStatement");
    ASSERT_EQ(varAssign->GetIdentifier()->GetLexeme(), "a",
              "Identifier should be 'a'");

    // The expression should be a primary expression representing "10".
    PrimaryExpression const *primExpr =
        dynamic_cast<PrimaryExpression const *>(varAssign->GetExpression());
    ASSERT_NE(primExpr, nullptr, "Expression should be a PrimaryExpression");
    ASSERT_EQ(primExpr->GetPrimary()->GetLexeme(), "10",
              "Literal should be '10'");
}

//--------------------------------------------------------------------------
// Test 3: Binary Expression Statement
// "1 + 2 * 3;"
TEST(ParserBinaryExpressionTest)
{
    std::string source = "1 + 2 * 3;";
    std::string filename = "test_binary_expr.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    // The statement should be an expression statement.
    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr,
              "Parsed statement should be an ExpressionStatement");

    // For "1 + 2 * 3", the top-level expression should be a binary expression
    // with '+'.
    BinaryExpression const *plusExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(plusExpr, nullptr,
              "Top-level expression should be a BinaryExpression");
    ASSERT_EQ(plusExpr->GetOperator()->GetLexeme(), "+",
              "Operator should be '+'");

    // The left operand should be the primary literal "1".
    PrimaryExpression const *leftPrim =
        dynamic_cast<PrimaryExpression const *>(plusExpr->GetLeftExpression());
    ASSERT_NE(leftPrim, nullptr, "Left operand should be a PrimaryExpression");
    ASSERT_EQ(leftPrim->GetPrimary()->GetLexeme(), "1",
              "Left literal should be '1'");

    // The right operand should be a binary expression representing "2 * 3".
    BinaryExpression const *multExpr =
        dynamic_cast<BinaryExpression const *>(plusExpr->GetRightExpression());
    ASSERT_NE(multExpr, nullptr,
              "Right operand should be a BinaryExpression for multiplication");
    ASSERT_EQ(multExpr->GetOperator()->GetLexeme(), "*",
              "Operator should be '*'");

    PrimaryExpression const *multLeft =
        dynamic_cast<PrimaryExpression const *>(multExpr->GetLeftExpression());
    ASSERT_NE(multLeft, nullptr,
              "Left operand of multiplication should be a PrimaryExpression");
    ASSERT_EQ(multLeft->GetPrimary()->GetLexeme(), "2",
              "Left literal of multiplication should be '2'");

    PrimaryExpression const *multRight =
        dynamic_cast<PrimaryExpression const *>(multExpr->GetRightExpression());
    ASSERT_NE(multRight, nullptr,
              "Right operand of multiplication should be a PrimaryExpression");
    ASSERT_EQ(multRight->GetPrimary()->GetLexeme(), "3",
              "Right literal of multiplication should be '3'");
}

//--------------------------------------------------------------------------
// Test 4: Grouping Expression Statement
// "(1 + 2) * 3;"
TEST(ParserGroupingExpressionTest)
{
    std::string source = "(1 + 2) * 3;";
    std::string filename = "test_grouping_expr.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr,
              "Parsed statement should be an ExpressionStatement");

    // Top-level expression should be a multiplication.
    BinaryExpression const *multExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(multExpr, nullptr,
              "Top-level expression should be a BinaryExpression");
    ASSERT_EQ(multExpr->GetOperator()->GetLexeme(), "*",
              "Operator should be '*'");

    // The left operand should be a grouping expression.
    GroupingExpression const *groupExpr =
        dynamic_cast<GroupingExpression const *>(multExpr->GetLeftExpression());
    ASSERT_NE(groupExpr, nullptr,
              "Left operand should be a GroupingExpression");

    // The grouping expression should contain a binary expression for "1 + 2".
    BinaryExpression const *innerPlus =
        dynamic_cast<BinaryExpression const *>(groupExpr->GetExpression());
    ASSERT_NE(innerPlus, nullptr,
              "Grouped expression should be a BinaryExpression");
    ASSERT_EQ(innerPlus->GetOperator()->GetLexeme(), "+",
              "Grouped operator should be '+'");

    PrimaryExpression const *innerLeft =
        dynamic_cast<PrimaryExpression const *>(innerPlus->GetLeftExpression());
    ASSERT_NE(innerLeft, nullptr,
              "Left operand of inner plus should be a PrimaryExpression");
    ASSERT_EQ(innerLeft->GetPrimary()->GetLexeme(), "1",
              "Left literal should be '1'");

    PrimaryExpression const *innerRight =
        dynamic_cast<PrimaryExpression const *>(
            innerPlus->GetRightExpression());
    ASSERT_NE(innerRight, nullptr,
              "Right operand of inner plus should be a PrimaryExpression");
    ASSERT_EQ(innerRight->GetPrimary()->GetLexeme(), "2",
              "Right literal should be '2'");

    // The right operand of the multiplication should be the primary literal
    // "3".
    PrimaryExpression const *rightPrim =
        dynamic_cast<PrimaryExpression const *>(multExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Right operand should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "3",
              "Right literal should be '3'");
}

//--------------------------------------------------------------------------
// Test 5: Cast Expression Statement
// "1 as i32;"
TEST(ParserCastExpressionTest)
{
    std::string source = "1 as i32;";
    std::string filename = "test_cast_expr.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1, "Should parse one statement");

    ExpressionStatement *exprStmt =
        dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr,
              "Parsed statement should be an ExpressionStatement");

    // The expression should be a cast expression.
    CastExpression const *castExpr =
        dynamic_cast<CastExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(castExpr, nullptr, "Expression should be a CastExpression");

    // The inner expression should be a primary literal "1".
    PrimaryExpression const *innerPrim =
        dynamic_cast<PrimaryExpression const *>(castExpr->GetExpression());
    ASSERT_NE(innerPrim, nullptr,
              "Inner expression should be a PrimaryExpression");
    ASSERT_EQ(innerPrim->GetPrimary()->GetLexeme(), "1",
              "Inner literal should be '1'");

    // The cast should use the datatype "i32".
    ASSERT_EQ(castExpr->GetDataType()->GetLexeme(), "i32",
              "Cast data type should be 'i32'");
}
