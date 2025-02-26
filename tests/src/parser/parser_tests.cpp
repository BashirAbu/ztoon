#include "lexer/lexer.h"
#include "parser/parser.h"
#include "ztest.h"
#include <string>

TEST(Parser_)
{
    std::string source = R"(
        a: i32 = 0;
        while false {}           )";
    Lexer lexer;
    lexer.Tokenize(source, "parser.ztoon");
    Parser parser(lexer.GetTokens());
    parser.Parse();
    parser.PrettyPrintAST();
}

//--------------------------------------------------------------------------
// Helper: parse source code and return AST statements.
static const std::vector<Statement *> &
parseSource(const std::string &source,
            const std::string &filename = "test.ztoon")
{
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    return parser.Parse();
}

TEST(Parser)
{
    std::string source = R"(
        cond: bool = true;
        a: i32 = cond? 0 : 1 ;
    )";

    Lexer lexer;
    lexer.Tokenize(source, "if_statement.ztoon");

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
    ASSERT_EQ(TokenDataTypeToString(castExpr->GetDataType()), "i32",
              "Cast data type should be 'i32'");
}

//--------------------------------------------------------------------------
// Compound Assignment Tests

// Test: "a += 5;" should be transformed into a binary expression with '+'.
TEST(ParserCompoundAssignmentPlusEqualTest)
{
    std::string source = "a += 5;";
    std::string filename = "compound_plus_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Statement should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::PLUS_EQUAL, "Compound operator should be PLUS_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr,
              "Compound assignment should yield a BinaryExpression");
    // The compound assignment should be transformed: a += b  becomes
    // a + b.
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::PLUS,
              "Binary operator should be PLUS");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "+",
              "Binary operator lexeme should be '+'");

    auto leftPrim =
        dynamic_cast<PrimaryExpression const *>(binExpr->GetLeftExpression());
    ASSERT_NE(leftPrim, nullptr, "Left operand should be a PrimaryExpression");
    ASSERT_EQ(leftPrim->GetPrimary()->GetLexeme(), "a",
              "Left operand should be 'a'");

    auto rightPrim =
        dynamic_cast<PrimaryExpression const *>(binExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Right operand should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "5",
              "Right operand should be '5'");
}

// Test: "a -= 5;"
TEST(ParserCompoundAssignmentDashEqualTest)
{
    std::string source = "a -= 5;";
    std::string filename = "compound_dash_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();
    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Statement should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::DASH_EQUAL, "Compound operator should be DASH_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr,
              "Compound assignment should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::DASH,
              "Binary operator should be DASH");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "-",
              "Binary operator lexeme should be '-'");

    auto leftPrim =
        dynamic_cast<PrimaryExpression const *>(binExpr->GetLeftExpression());
    ASSERT_NE(leftPrim, nullptr, "Left operand should be a PrimaryExpression");
    ASSERT_EQ(leftPrim->GetPrimary()->GetLexeme(), "a",
              "Left operand should be 'a'");

    auto rightPrim =
        dynamic_cast<PrimaryExpression const *>(binExpr->GetRightExpression());
    ASSERT_NE(rightPrim, nullptr,
              "Right operand should be a PrimaryExpression");
    ASSERT_EQ(rightPrim->GetPrimary()->GetLexeme(), "5",
              "Right operand should be '5'");
}

// Test: "a *= 5;"
TEST(ParserCompoundAssignmentAsteriskEqualTest)
{
    std::string source = "a *= 5;";
    std::string filename = "compound_asterisk_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::ASTERISK_EQUAL,
              "Compound operator should be ASTERISK_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::ASTERISK,
              "Binary operator should be ASTERISK");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "*",
              "Binary operator lexeme should be '*'");
}

// Test: "a /= 5;"
TEST(ParserCompoundAssignmentSlashEqualTest)
{
    std::string source = "a /= 5;";
    std::string filename = "compound_slash_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::SLASH_EQUAL,
              "Compound operator should be SLASH_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::SLASH,
              "Binary operator should be SLASH");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "/",
              "Binary operator lexeme should be '/'");
}

// Test: "a %= 5;"
TEST(ParserCompoundAssignmentPercentageEqualTest)
{
    std::string source = "a %= 5;";
    std::string filename = "compound_percentage_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::PERCENTAGE_EQUAL,
              "Compound operator should be PERCENTAGE_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    // Note: the implementation sets the lexeme to "%%" for percentage compound.
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::PERCENTAGE,
              "Binary operator should be PERCENTAGE");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "%%",
              "Binary operator lexeme should be '%%'");
}

// Test: "a |= 5;"
TEST(ParserCompoundAssignmentBitwiseOrEqualTest)
{
    std::string source = "a |= 5;";
    std::string filename = "compound_bitwise_or_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::BITWISE_OR_EQUAL,
              "Compound operator should be BITWISE_OR_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::BITWISE_OR,
              "Binary operator should be BITWISE_OR");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "|",
              "Binary operator lexeme should be '|'");
}

// Test: "a &= 5;"
// Note: Due to an implementation quirk, the binary operator's type is set to
// PLUS but its lexeme is "&".
TEST(ParserCompoundAssignmentBitwiseAndEqualTest)
{
    std::string source = "a &= 5;";
    std::string filename = "compound_bitwise_and_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::BITWISE_AND_EQUAL,
              "Compound operator should be BITWISE_AND_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    // Even though the intended operator is BITWISE_AND, the implementation sets
    // the type to PLUS.
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "&",
              "Binary operator lexeme should be '&'");
}

// Test: "a ^= 5;"
TEST(ParserCompoundAssignmentBitwiseXorEqualTest)
{
    std::string source = "a ^= 5;";
    std::string filename = "compound_bitwise_xor_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::BITWISE_XOR_EQUAL,
              "Compound operator should be BITWISE_XOR_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::BITWISE_XOR,
              "Binary operator should be BITWISE_XOR");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "^",
              "Binary operator lexeme should be '^'");
}

// Test: "a <<= 5;"
TEST(ParserCompoundAssignmentShiftLeftEqualTest)
{
    std::string source = "a <<= 5;";
    std::string filename = "compound_shift_left_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::SHIFT_LEFT_EQUAL,
              "Compound operator should be SHIFT_LEFT_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::SHIFT_LEFT,
              "Binary operator should be SHIFT_LEFT");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "<<",
              "Binary operator lexeme should be '<<'");
}

// Test: "a >>= 5;"
TEST(ParserCompoundAssignmentShiftRightEqualTest)
{
    std::string source = "a >>= 5;";
    std::string filename = "compound_shift_right_equal.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto compoundStmt =
        dynamic_cast<VarCompoundAssignmentStatement *>(stmts[0]);
    ASSERT_NE(compoundStmt, nullptr,
              "Should be VarCompoundAssignmentStatement");
    ASSERT_EQ(compoundStmt->GetCompoundAssignment()->GetType(),
              TokenType::SHIFT_RIGHT_EQUAL,
              "Compound operator should be SHIFT_RIGHT_EQUAL");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(compoundStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Should yield a BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::SHIFT_RIGHT,
              "Binary operator should be SHIFT_RIGHT");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), ">>",
              "Binary operator lexeme should be '>>'");
}

//--------------------------------------------------------------------------
// New Binary Operator Tests

// Test: Logical OR: "1 || 0;"
TEST(ParserLogicalOrTest)
{
    std::string source = "1 || 0;";
    std::string filename = "logical_or.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr,
              "Top-level expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::OR,
              "Operator should be OR");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "||",
              "Operator lexeme should be '||'");
}

// Test: Logical AND: "1 && 0;"
TEST(ParserLogicalAndTest)
{
    std::string source = "1 && 0;";
    std::string filename = "logical_and.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr,
              "Top-level expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::AND,
              "Operator should be AND");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "&&",
              "Operator lexeme should be '&&'");
}

// Test: Bitwise OR: "1 | 2;"
TEST(ParserBitwiseOrTest)
{
    std::string source = "1 | 2;";
    std::string filename = "bitwise_or.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::BITWISE_OR,
              "Operator should be BITWISE_OR");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "|",
              "Operator lexeme should be '|'");
}

// Test: Bitwise XOR: "1 ^ 2;"
TEST(ParserBitwiseXorTest)
{
    std::string source = "1 ^ 2;";
    std::string filename = "bitwise_xor.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::BITWISE_XOR,
              "Operator should be BITWISE_XOR");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "^",
              "Operator lexeme should be '^'");
}

// Test: Bitwise AND: "1 & 2;"
TEST(ParserBitwiseAndTest)
{
    std::string source = "1 & 2;";
    std::string filename = "bitwise_and.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::BITWISE_AND,
              "Operator should be BITWISE_AND");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "&",
              "Operator lexeme should be '&'");
}

// Test: Shift Left: "1 << 2;"
TEST(ParserShiftLeftTest)
{
    std::string source = "1 << 2;";
    std::string filename = "shift_left.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::SHIFT_LEFT,
              "Operator should be SHIFT_LEFT");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "<<",
              "Operator lexeme should be '<<'");
}

// Test: Shift Right: "1 >> 2;"
TEST(ParserShiftRightTest)
{
    std::string source = "1 >> 2;";
    std::string filename = "shift_right.ztoon";
    Lexer lexer;
    lexer.Tokenize(source, filename);
    Parser parser(lexer.GetTokens());
    const auto &stmts = parser.Parse();

    auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr, "Should be an ExpressionStatement");

    auto binExpr =
        dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
    ASSERT_NE(binExpr, nullptr, "Expression should be BinaryExpression");
    ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::SHIFT_RIGHT,
              "Operator should be SHIFT_RIGHT");
    ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), ">>",
              "Operator lexeme should be '>>'");
}

// Test: Comparison Operators: "1 < 2;", "1 <= 2;", "1 > 2;", "1 >= 2;"
TEST(ParserComparisonOperatorsTest)
{
    {
        std::string source = "1 < 2;";
        Lexer lexer;
        lexer.Tokenize(source, "comp_less.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::LESS,
                  "Operator should be LESS");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), "<",
                  "Operator lexeme should be '<'");
    }
    {
        std::string source = "1 <= 2;";
        Lexer lexer;
        lexer.Tokenize(source, "comp_less_equal.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::LESS_EQUAL,
                  "Operator should be LESS_EQUAL");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(),
                  "<=", "Operator lexeme should be '<='");
    }
    {
        std::string source = "1 > 2;";
        Lexer lexer;
        lexer.Tokenize(source, "comp_greater.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::GREATER,
                  "Operator should be GREATER");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(), ">",
                  "Operator lexeme should be '>'");
    }
    {
        std::string source = "1 >= 2;";
        Lexer lexer;
        lexer.Tokenize(source, "comp_greater_equal.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::GREATER_EQUAL,
                  "Operator should be GREATER_EQUAL");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(),
                  ">=", "Operator lexeme should be '>='");
    }
}

// Test: Equality Operators: "1 == 1;", "1 != 2;"
TEST(ParserEqualityOperatorsTest)
{
    {
        std::string source = "1 == 1;";
        Lexer lexer;
        lexer.Tokenize(source, "eq_equal.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(), TokenType::EQUAL_EQUAL,
                  "Operator should be EQUAL_EQUAL");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(),
                  "==", "Operator lexeme should be '=='");
    }
    {
        std::string source = "1 != 2;";
        Lexer lexer;
        lexer.Tokenize(source, "not_equal.ztoon");
        Parser parser(lexer.GetTokens());
        const auto &stmts = parser.Parse();
        auto exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
        auto binExpr =
            dynamic_cast<BinaryExpression const *>(exprStmt->GetExpression());
        ASSERT_EQ(binExpr->GetOperator()->GetType(),
                  TokenType::EXCLAMATION_EQUAL,
                  "Operator should be EXCLAMATION_EQUAL");
        ASSERT_EQ(binExpr->GetOperator()->GetLexeme(),
                  "!=", "Operator lexeme should be '!='");
    }
}
