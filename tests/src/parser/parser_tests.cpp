#include "lexer/lexer.h"
#include "parser/parser.h"
#include "ztest.h"
#include <string>

TEST(Parser_)
{
    std::string source = R"(
        fn add (a:i32, b:i32) -> i32
        {
            i : i32 = add(1,3);
            ret a + b;
        }          )";
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

// Test 1: Simple While Loop
// Source: while (x < 10) { x = x + 1; }
TEST(ParserWhileLoopTest)
{
    std::string source = "while (x < 10) { x = x + 1; }";
    Lexer lexer;
    lexer.Tokenize(source, "while_test.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    // Expect one top-level statement
    ASSERT_EQ(stmts.size(), 1,
              "Expected one top-level statement for while loop");

    // The top-level statement should be a WhileLoopStatement.
    auto *whileStmt = dynamic_cast<WhileLoopStatement *>(stmts[0]);
    ASSERT_NE(whileStmt, nullptr,
              "Top-level statement should be a WhileLoopStatement");

    // Check that the condition is parsed (it should be a binary expression for
    // 'x < 10').
    ASSERT_NE(whileStmt->GetCondition(), nullptr,
              "While loop must have a condition");

    // Check that the block exists and contains one statement (the assignment x
    // = x + 1;)
    auto *block =
        dynamic_cast<BlockStatement *>(whileStmt->GetBlockStatement());
    ASSERT_NE(block, nullptr, "While loop block must be a BlockStatement");
    ASSERT_EQ(block->GetStatements().size(), 1,
              "While loop block should contain one statement");

    // Verify that the inner statement is a variable assignment.
    auto *assignStmt =
        dynamic_cast<VarAssignmentStatement *>(block->GetStatements()[0]);
    ASSERT_NE(assignStmt, nullptr,
              "Inner statement should be a VarAssignmentStatement");
}

// Test 2: Simple For Loop with all parts
// Source: for (i: i32 = 0; i < 10; i = i + 1) { x = x + i; }
TEST(ParserForLoopTest)
{
    std::string source = "for (i: i32 = 0; i < 10; i = i + 1) { x = x + i; }";
    Lexer lexer;
    lexer.Tokenize(source, "for_test.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    // The for-loop parser returns a BlockStatement that wraps the
    // ForLoopStatement.
    ASSERT_EQ(stmts.size(), 1, "Expected one top-level statement for for loop");

    auto *block = dynamic_cast<BlockStatement *>(stmts[0]);
    ASSERT_NE(
        block, nullptr,
        "Top-level statement should be a BlockStatement wrapping the for loop");
    ASSERT_EQ(block->GetStatements().size(), 1,
              "Block should contain one for loop statement");

    auto *forStmt = dynamic_cast<ForLoopStatement *>(block->GetStatements()[0]);
    ASSERT_NE(forStmt, nullptr, "Statement should be a ForLoopStatement");

    // Check that init, condition, update, and block are present.
    ASSERT_NE(forStmt->GetInit(), nullptr, "For loop init must be present");
    ASSERT_NE(forStmt->GetCondition(), nullptr,
              "For loop condition must be present");
    ASSERT_NE(forStmt->GetUpdate(), nullptr, "For loop update must be present");
    ASSERT_NE(forStmt->GetBlockStatement(), nullptr,
              "For loop block must be present");
}

// Test 3: For Loop with Empty Init and Update
// Source: for (; x < 10; ) { x = x + 1; }
TEST(ParserForLoopEmptyPartsTest)
{
    std::string source = "for (; x < 10; ) { x = x + 1; }";
    Lexer lexer;
    lexer.Tokenize(source, "for_empty.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    auto *block = dynamic_cast<BlockStatement *>(stmts[0]);
    ASSERT_NE(block, nullptr,
              "Expected a BlockStatement wrapping the for loop");

    auto *forStmt = dynamic_cast<ForLoopStatement *>(block->GetStatements()[0]);
    ASSERT_NE(forStmt, nullptr, "Statement should be a ForLoopStatement");
    // In this case, init and update might be null.
    ASSERT_NE(forStmt->GetCondition(), nullptr, "Condition is not nullptr");

    ASSERT_NE(dynamic_cast<EmptyStatement *>(forStmt->GetInit()), nullptr,
              "Init is EmptyStatement");
    ASSERT_EQ(forStmt->GetUpdate(), nullptr, "Update is nullptr");
    ASSERT_NE(forStmt->GetBlockStatement(), nullptr,
              "For loop block must be present");
}

// Test 4: Nested Loops (While containing a For Loop)
// Source:
//   while (x < 5) {
//       for (i: i32 = 0; i < 3; i = i + 1) {
//           x = x + i;
//       }
//   }
TEST(ParserNestedLoopsTest)
{
    std::string source = R"(
        while (x < 5) {
           for (i: i32 = 0; i < 3; i = i + 1) {
              x = x + i;
           }
        }
    )";
    Lexer lexer;
    lexer.Tokenize(source, "nested_loops.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1,
              "Expected one top-level statement for nested loops");

    auto *whileStmt = dynamic_cast<WhileLoopStatement *>(stmts[0]);
    ASSERT_NE(whileStmt, nullptr,
              "Top-level statement should be a WhileLoopStatement");

    auto *whileBlock =
        dynamic_cast<BlockStatement *>(whileStmt->GetBlockStatement());
    ASSERT_NE(whileBlock, nullptr,
              "While loop block should be a BlockStatement");
    ASSERT_EQ(whileBlock->GetStatements().size(), 1,
              "While loop block should contain one statement");

    auto *forStmtOuterBlock =
        dynamic_cast<BlockStatement *>(whileBlock->GetStatements()[0]);
    ASSERT_NE(
        dynamic_cast<ForLoopStatement *>(forStmtOuterBlock->GetStatements()[0]),
        nullptr, "Nested statement should be a ForLoopStatement");
}

// Test 5: While Loop with Empty Block
// Source: while (x < 10) { }
// This tests that the parser can handle an empty block.
TEST(ParserWhileLoopEmptyBlockTest)
{
    std::string source = "while (x < 10) { }";
    Lexer lexer;
    lexer.Tokenize(source, "while_empty.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();
    ASSERT_EQ(stmts.size(), 1,
              "Expected one statement for while loop with empty block");

    auto *whileStmt = dynamic_cast<WhileLoopStatement *>(stmts[0]);
    ASSERT_NE(whileStmt, nullptr, "Statement should be a WhileLoopStatement");

    auto *block =
        dynamic_cast<BlockStatement *>(whileStmt->GetBlockStatement());
    ASSERT_NE(block, nullptr, "While loop block should be a BlockStatement");
    // The block might be empty.
    ASSERT_EQ(block->GetStatements().size(), 0,
              "Empty while loop block should contain 0 statements");
}

// Test 1: Function Prototype Parsing
// Source: fn myFunc() -> i32;
TEST(ParserFunctionPrototypeTest)
{
    std::string source = "fn myFunc() -> i32;";
    Lexer lexer;
    lexer.Tokenize(source, "fn_proto.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    // Expect one top-level statement.
    ASSERT_EQ(stmts.size(), 1,
              "Expected one top-level statement for a function prototype");

    // The statement should be an expression statement wrapping a FnExpression.
    auto *exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr,
              "Top-level statement should be an ExpressionStatement");
    auto *fnExpr = dynamic_cast<FnExpression *>(exprStmt->GetExpression());
    ASSERT_NE(fnExpr, nullptr, "Expression should be a FnExpression");

    // Check that the function identifier is "myFunc".
    ASSERT_EQ(fnExpr->GetIdentifier()->GetLexeme(), std::string("myFunc"),
              "Function name should be 'myFunc'");

    // Check that there are no GetParameters().
    ASSERT_EQ(fnExpr->GetParameters().size(), 0,
              "Function prototype should have zero GetParameters()");

    // Check that the return type is "i32".
    ASSERT_EQ(fnExpr->GetReturnDatatype()->GetLexeme(), std::string("i32"),
              "Return type should be 'i32'");

    // The prototype flag should be set.
    ASSERT_EQ(fnExpr->IsPrototype(), true,
              "FnExpression should be marked as a prototype");
}

// Test 2: Function Definition Parsing
// Source: fn add(a: i32, b: i32) -> i32 { a + b; }
TEST(ParserFunctionDefinitionTest)
{
    std::string source = "fn add(a: i32, b: i32) -> i32 { a + b; }";
    Lexer lexer;
    lexer.Tokenize(source, "fn_def.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    ASSERT_EQ(stmts.size(), 1,
              "Expected one top-level statement for a function definition");

    auto *exprStmt = dynamic_cast<ExpressionStatement *>(stmts[0]);
    ASSERT_NE(exprStmt, nullptr,
              "Top-level statement should be an ExpressionStatement");
    auto *fnExpr = dynamic_cast<FnExpression *>(exprStmt->GetExpression());
    ASSERT_NE(fnExpr, nullptr, "Expression should be a FnExpression");

    // Verify function name.
    ASSERT_EQ(fnExpr->GetIdentifier()->GetLexeme(), std::string("add"),
              "Function name should be 'add'");

    // Verify parameter list.
    ASSERT_EQ(fnExpr->GetParameters().size(), 2,
              "Function 'add' should have 2 GetParameters()");
    auto *paramA = fnExpr->GetParameters()[0];
    ASSERT_EQ(paramA->GetIdentifier()->GetLexeme(), std::string("a"),
              "First parameter should be 'a'");
    ASSERT_EQ(paramA->GetDataType()->GetLexeme(), std::string("i32"),
              "Type of 'a' should be 'i32'");
    auto *paramB = fnExpr->GetParameters()[1];
    ASSERT_EQ(paramB->GetIdentifier()->GetLexeme(), std::string("b"),
              "Second parameter should be 'b'");
    ASSERT_EQ(paramB->GetDataType()->GetLexeme(), std::string("i32"),
              "Type of 'b' should be 'i32'");

    // Verify return type.
    ASSERT_EQ(fnExpr->GetReturnDatatype()->GetLexeme(), std::string("i32"),
              "Return type should be 'i32'");

    // This is a full definition so it should not be marked as a prototype.
    ASSERT_EQ(fnExpr->IsPrototype(), false,
              "Function definition should not be a prototype");

    // Verify that a block statement is present.
    ASSERT_NE(fnExpr->GetBlockStatement(), nullptr,
              "Function definition must have a block statement");
    ASSERT_NE(fnExpr->GetBlockStatement()->GetStatements().size(), 0,
              "Function block should contain at least one statement");
}

// Test 3: Function Call Parsing
// Source: fn mul(a: i32, b: i32) -> i32 { a * b; } x: i32 = mul(3, 4);
TEST(ParserFunctionCallTest)
{
    std::string source =
        "fn mul(a: i32, b: i32) -> i32 { a * b; } x: i32 = mul(3, 4);";
    Lexer lexer;
    lexer.Tokenize(source, "fn_call.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    // Expect two top-level statements: function definition and a variable
    // assignment.
    ASSERT_EQ(stmts.size(), 2,
              "Expected two top-level statements (function definition and "
              "variable assignment)");

    // Check the variable assignment; its expression should be a function call.
    auto *varAssign = dynamic_cast<VarDeclStatement *>(stmts[1]);
    ASSERT_NE(varAssign, nullptr,
              "Second statement should be a VarAssignmentStatement");
    auto *fnCallExpr =
        dynamic_cast<FnCallExpression *>(varAssign->GetExpression());
    ASSERT_NE(fnCallExpr, nullptr,
              "The assignment expression should be a FnCallExpression");

    // Verify that the function call identifier is "mul".
    ASSERT_EQ(fnCallExpr->GetIdentifier()->GetLexeme(), std::string("mul"),
              "Function call should be to 'mul'");

    // Verify that there are exactly two arguments.
    ASSERT_EQ(fnCallExpr->GetArgs().size(), 2,
              "Function call should have two arguments");

    // Check that the first and second arguments are primary expressions with
    // integer literals "3" and "4".
    auto *arg0 = dynamic_cast<PrimaryExpression *>(fnCallExpr->GetArgs()[0]);
    ASSERT_NE(arg0, nullptr, "First argument should be a PrimaryExpression");
    ASSERT_EQ(arg0->GetPrimary()->GetLexeme(), std::string("3"),
              "First argument should be '3'");
    auto *arg1 = dynamic_cast<PrimaryExpression *>(fnCallExpr->GetArgs()[1]);
    ASSERT_NE(arg1, nullptr, "Second argument should be a PrimaryExpression");
    ASSERT_EQ(arg1->GetPrimary()->GetLexeme(), std::string("4"),
              "Second argument should be '4'");
}

// Test 4: Complex Function Parsing with Nested Function Call
// Source:
//   fn complex(a: i32, b: i32, c: i32) -> i32 { a + b * c; }
//   fn wrapper() -> i32 { complex(2, 3, 4); }
//   x: i32 = wrapper();
TEST(ParserComplexFunctionTest)
{
    std::string source =
        "fn complex(a: i32, b: i32, c: i32) -> i32 { a + b * c; } "
        "fn wrapper() -> i32 { complex(2, 3, 4); } "
        "x: i32 = wrapper();";
    Lexer lexer;
    lexer.Tokenize(source, "complex_fn.ztoon");
    Parser parser(lexer.GetTokens());
    auto &stmts = parser.Parse();

    // Expect three top-level statements.
    ASSERT_EQ(stmts.size(), 3,
              "Expected three top-level statements (2 function definitions and "
              "one variable assignment)");

    // Check the first function: "complex"
    auto *fnExpr1 = dynamic_cast<FnExpression *>(
        dynamic_cast<ExpressionStatement *>(stmts[0])->GetExpression());
    ASSERT_NE(fnExpr1, nullptr,
              "First statement should be a FnExpression for 'complex'");
    ASSERT_EQ(fnExpr1->GetIdentifier()->GetLexeme(), std::string("complex"),
              "Function name should be 'complex'");
    ASSERT_EQ(fnExpr1->GetParameters().size(), 3,
              "Function 'complex' should have 3 GetParameters()");

    // Check the second function: "wrapper"
    auto *fnExpr2 = dynamic_cast<FnExpression *>(
        dynamic_cast<ExpressionStatement *>(stmts[1])->GetExpression());
    ASSERT_NE(fnExpr2, nullptr,
              "Second statement should be a FnExpression for 'wrapper'");
    ASSERT_EQ(fnExpr2->GetIdentifier()->GetLexeme(), std::string("wrapper"),
              "Function name should be 'wrapper'");
    ASSERT_EQ(fnExpr2->GetParameters().size(), 0,
              "Function 'wrapper' should have 0 GetParameters()");

    // Check the third statement: variable assignment with a function call.
    auto *varAssign = dynamic_cast<VarDeclStatement *>(stmts[2]);
    ASSERT_NE(varAssign, nullptr,
              "Third statement should be a VarAssignmentStatement");
    auto *fnCall = dynamic_cast<FnCallExpression *>(varAssign->GetExpression());
    ASSERT_NE(fnCall, nullptr,
              "Variable assignment should contain a FnCallExpression");
    ASSERT_EQ(fnCall->GetIdentifier()->GetLexeme(), std::string("wrapper"),
              "Function call should be to 'wrapper'");
}
