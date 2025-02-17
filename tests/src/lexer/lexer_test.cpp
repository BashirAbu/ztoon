#include "lexer/lexer.h"
#include "ztest.h"
#include <fstream>
#include <sstream>

TEST(LexerVariableDeclarationTest)
{
    std::fstream file("source_code/lexer_variabel_declaration_test.ztoon");
    ASSERT_EQ(file.is_open(), true,
              std::format("Failed to open file",
                          "source_code/lexer_variabel_declaration_test.ztoon"));
    std::string source = (std::stringstream() << file.rdbuf()).str();
    Lexer lexer;
    lexer.Tokenize(source, "source_code/lexer_variabel_declaration_test.ztoon");

    ASSERT_EQ(lexer.GetTokens().at(0)->GetLexeme(), (std::string) "var",
              "Token lexeme should be 'var'");

    ASSERT_EQ(lexer.GetTokens().at(0)->GetLineNumber(), 3,
              "Token token line number shoulbe be 3");

    ASSERT_EQ(lexer.GetTokens().at(0)->GetColNumber(), 1,
              "Token column number should be 1");
    ASSERT_EQ(lexer.GetTokens().at(0)->GetFilename(),
              (std::string) "source_code/lexer_variabel_declaration_test.ztoon",
              "Token file name should be "
              "source_code/lexer_variabel_declaration_test.ztoon");
    ASSERT_EQ(lexer.GetTokens().at(0)->GetType(), TokenType::IDENTIFIER,
              "Token type should be IDENTIFIER");

    ASSERT_EQ(lexer.GetTokens().at(0)->GetLineStr(),
              (std::string)("var : i32 = 12;"),
              "Token code line should be var : i32 = 12;");

    ASSERT_EQ(lexer.GetTokens().at(10)->GetLexeme(), (std::string) "1.3",
              "Token lexeme should be '1.3'");

    ASSERT_EQ(lexer.GetTokens().at(10)->GetLineNumber(), 4,
              "Token token line number shoulbe be 4");

    ASSERT_EQ(lexer.GetTokens().at(10)->GetColNumber(), 13,
              "Token column number should be 13");
    ASSERT_EQ(lexer.GetTokens().at(10)->GetFilename(),
              (std::string) "source_code/lexer_variabel_declaration_test.ztoon",
              "Token file name should be "
              "source_code/lexer_variabel_declaration_test.ztoon");

    ASSERT_EQ(lexer.GetTokens().at(10)->GetLineStr(),
              (std::string)("fvar: f32 = 1.3;"),
              "Token code line should be fvar: f32 = 1.3;");

    ASSERT_EQ(lexer.GetTokens().at(10)->GetType(), TokenType::FLOAT_LITERAL,
              "Token type should be FLOAT_LITERAL");
}

//---------------------------------------------------------
// Basic Token Tests
//---------------------------------------------------------

// Test that a simple keyword token ("i8") is recognized correctly.
TEST(LexerSimpleTokenTest)
{
    Lexer lexer;
    std::string input = "i8";
    std::string filename = "test1";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1, "There should be 1 token for input 'i8'");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::I8, "Token type should be I8");
    ASSERT_EQ(token->GetLexeme(), "i8", "Token lexeme should be 'i8'");
    ASSERT_EQ(token->GetFilename(), filename,
              "Token filename should be set correctly");
    ASSERT_EQ(token->GetLineNumber(), 1, "Token line number should be 1");
}

// Test that numeric literals are parsed as INTEGER_LITERAL tokens.
TEST(LexerNumericLiteralTest)
{
    Lexer lexer;
    std::string input = "123";
    std::string filename = "test_numeric";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1, "There should be 1 token for numeric literal");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::INTEGER_LITERAL,
              "Token type should be INTEGER_LITERAL");
    auto *intToken = dynamic_cast<TokenLiteral<uint64_t> *>(token);
    ASSERT_NE(intToken, nullptr,
              "Token should be of type TokenLiteral<uint64_t>");
    ASSERT_EQ(intToken->GetValue(), 123, "Integer literal value should be 123");
}

// Test that float literals are parsed as FLOAT_LITERAL tokens.
TEST(LexerFloatLiteralTest)
{
    Lexer lexer;
    std::string input = "3.14";
    std::string filename = "test_float";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1, "There should be 1 token for float literal");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::FLOAT_LITERAL,
              "Token type should be FLOAT_LITERAL");
    auto *floatToken = dynamic_cast<TokenLiteral<double> *>(token);
    ASSERT_NE(floatToken, nullptr,
              "Token should be of type TokenLiteral<double>");
    ASSERT_EQ(floatToken->GetValue(), 3.14,
              "Float literal value should be 3.14");
}

// Test that string literals are parsed as STRING_LITERAL tokens.
TEST(LexerStringLiteralTest)
{
    Lexer lexer;
    std::string input = "\"hello\"";
    std::string filename = "test_string";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1, "There should be 1 token for string literal");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::STRING_LITERAL,
              "Token type should be STRING_LITERAL");
    auto *strToken = dynamic_cast<TokenLiteral<std::string> *>(token);
    ASSERT_NE(strToken, nullptr,
              "Token should be of type TokenLiteral<std::string>");
    ASSERT_EQ(strToken->GetValue(), "\"hello\"",
              "String literal value should be '\"hello\"'");
}

// Test that character literals are parsed as CHARACTER_LITERAL tokens.
TEST(LexerCharacterLiteralTest)
{
    Lexer lexer;
    std::string input = "'a'";
    std::string filename = "test_char";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1,
              "There should be 1 token for character literal");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::CHARACTER_LITERAL,
              "Token type should be CHARACTER_LITERAL");
    auto *charToken = dynamic_cast<TokenLiteral<int8_t> *>(token);
    ASSERT_NE(charToken, nullptr,
              "Token should be of type TokenLiteral<int8_t>");
    ASSERT_EQ(charToken->GetValue(), 'a',
              "Character literal value should be 'a'");
}

// Test that boolean literals ("true" and "false") are recognized.
TEST(LexerBooleanLiteralTest)
{
    Lexer lexer;
    std::string input = "true false";
    std::string filename = "test_bool";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 2,
              "There should be 2 tokens for boolean literals");
    Token *token1 = tokens[0];
    Token *token2 = tokens[1];
    ASSERT_EQ(token1->GetType(), TokenType::TRUE, "First token should be TRUE");
    ASSERT_EQ(token2->GetType(), TokenType::FALSE,
              "Second token should be FALSE");
    auto *boolToken1 = dynamic_cast<TokenLiteral<bool> *>(token1);
    auto *boolToken2 = dynamic_cast<TokenLiteral<bool> *>(token2);
    ASSERT_NE(boolToken1, nullptr,
              "First token should be of type TokenLiteral<bool>");
    ASSERT_NE(boolToken2, nullptr,
              "Second token should be of type TokenLiteral<bool>");
    ASSERT_EQ(boolToken1->GetValue(), true,
              "Boolean literal value should be true");
    ASSERT_EQ(boolToken2->GetValue(), false,
              "Boolean literal value should be false");
}

// Test that identifiers are tokenized properly.
TEST(LexerIdentifierTest)
{
    Lexer lexer;
    std::string input = "variableName";
    std::string filename = "test_identifier";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 1, "There should be 1 token for identifier");
    Token *token = tokens[0];
    ASSERT_EQ(token->GetType(), TokenType::IDENTIFIER,
              "Token type should be IDENTIFIER");
    ASSERT_EQ(token->GetLexeme(), "variableName",
              "Token lexeme should match the identifier");
}

// Test a combination of tokens from a simple declaration.
TEST(LexerMultipleTokensTest)
{
    Lexer lexer;
    std::string input = "i32 x = 100;";
    std::string filename = "test_multiple";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    // Expected tokens: "i32" (I32), "x" (IDENTIFIER), "=" (EQUAL), "100"
    // (INTEGER_LITERAL), ";" (SEMICOLON)
    ASSERT_EQ(tokens.size(), 5, "There should be 5 tokens for 'i32 x = 100;'");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I32,
              "First token should be I32");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::IDENTIFIER,
              "Second token should be IDENTIFIER");
    ASSERT_EQ(tokens[2]->GetType(), TokenType::EQUAL,
              "Third token should be EQUAL");
    ASSERT_EQ(tokens[3]->GetType(), TokenType::INTEGER_LITERAL,
              "Fourth token should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[4]->GetType(), TokenType::SEMICOLON,
              "Fifth token should be SEMICOLON");
}

// Test that line and column numbers are tracked correctly.
TEST(LexerLineColumnTest)
{
    Lexer lexer;
    std::string input = "i8\ni16";
    std::string filename = "test_line_column";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 2, "There should be 2 tokens for 'i8\\ni16'");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I8, "First token should be I8");
    ASSERT_EQ(tokens[0]->GetLineNumber(), 1, "First token should be on line 1");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::I16,
              "Second token should be I16");
    ASSERT_EQ(tokens[1]->GetLineNumber(), 2,
              "Second token should be on line 2");
}

// Test that single-line comments are skipped.
TEST(LexerCommentSkippingTest)
{
    Lexer lexer;
    std::string input = "i32 // this is a comment\n123";
    std::string filename = "test_comment";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    // Comments should be skipped, leaving tokens for "i32" and "123"
    ASSERT_EQ(tokens.size(), 2,
              "Comments should be skipped, only tokens for 'i32' and '123' "
              "should remain");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I32,
              "First token should be I32");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::INTEGER_LITERAL,
              "Second token should be INTEGER_LITERAL");
}

// Test that multi-line comments are skipped.
TEST(LexerMultiLineCommentTest)
{
    Lexer lexer;
    std::string input = "i32 /* multi-line\n comment */ 456";
    std::string filename = "test_multiline_comment";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    // Multi-line comment should be skipped, leaving tokens for "i32" and "456"
    ASSERT_EQ(tokens.size(), 2, "Multi-line comment should be skipped");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I32,
              "First token should be I32");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::INTEGER_LITERAL,
              "Second token should be INTEGER_LITERAL");
}

//---------------------------------------------------------
// Additional Edge Cases
//---------------------------------------------------------

// Test that an empty input produces no tokens.
TEST(LexerEmptyInputTest)
{
    Lexer lexer;
    std::string input = "";
    std::string filename = "empty";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 0, "Empty input should produce 0 tokens");
}

// Test that input containing only whitespace and newlines produces no tokens.
TEST(LexerWhitespaceOnlyTest)
{
    Lexer lexer;
    std::string input = "    \n  \t  ";
    std::string filename = "whitespace";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    ASSERT_EQ(tokens.size(), 0,
              "Whitespace only input should produce 0 tokens");
}

// Test a complex input that mixes different token types and comments.
TEST(LexerComplexMixedTokensTest)
{
    Lexer lexer;
    std::string input = "i32 x = 100; // declare x\n"
                        "i16 y = 200;\n"
                        "\"hello world\"\n"
                        "'i'";
    std::string filename = "complex_test";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();
    // Expected tokens:
    // Line 1: i32, x, =, 100, ;
    // Line 2: i16, y, =, 200, ;
    // Line 3: "hello world"
    // Line 4: 'i'
    ASSERT_EQ(tokens.size(), 12, "Complex input should produce 12 tokens");

    // Verify a few tokens from different lines.
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I32, "Token 0 should be I32");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::IDENTIFIER,
              "Token 1 should be IDENTIFIER");
    ASSERT_EQ(tokens[3]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 3 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[5]->GetType(), TokenType::I16, "Token 5 should be I16");
    ASSERT_EQ(tokens[8]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 8 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[11]->GetType(), TokenType::CHARACTER_LITERAL,
              "Token 11 should be CHARACTER_LITERAL");
}

//---------------------------------------------------------
// Operator Token Tests
//---------------------------------------------------------

// Test that individual operator tokens are recognized correctly.
TEST(LexerOperatorTokensTest)
{
    Lexer lexer;
    // Each operator is separated by whitespace.
    std::string input = "* / + - % as";
    std::string filename = "test_operator";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();

    // Expect 6 tokens in the order: ASTERISK, SLASH, PLUS, DASH, PERCENTAGE,
    // AS.
    ASSERT_EQ(tokens.size(), 6,
              "There should be 6 tokens for the operator input");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::ASTERISK,
              "Token 0 should be ASTERISK");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::SLASH,
              "Token 1 should be SLASH");
    ASSERT_EQ(tokens[2]->GetType(), TokenType::PLUS, "Token 2 should be PLUS");
    ASSERT_EQ(tokens[3]->GetType(), TokenType::DASH, "Token 3 should be DASH");
    ASSERT_EQ(tokens[4]->GetType(), TokenType::PERCENTAGE,
              "Token 4 should be PERCENTAGE");
    ASSERT_EQ(tokens[5]->GetType(), TokenType::AS, "Token 5 should be AS");
}

// Test operators in a mixed expression to ensure they interact correctly with
// other tokens.
TEST(LexerMixedOperatorsTest)
{
    Lexer lexer;
    // Example: a declaration and initialization using various operators.
    std::string input = "i32 a = 10 * 2.0 + 5 - 3 % 2 as i32;";
    std::string filename = "test_mixed_operators";
    lexer.Tokenize(input, filename);
    const auto &tokens = lexer.GetTokens();

    // Expected tokens breakdown:
    //   0: "i32"               -> I32
    //   1: "a"                 -> IDENTIFIER
    //   2: "="                 -> EQUAL
    //   3: "10"                -> INTEGER_LITERAL
    //   4: "*"                 -> ASTERISK
    //   5: "2"                 -> INTEGER_LITERAL
    //   6: "+"                 -> PLUS
    //   7: "5"                 -> INTEGER_LITERAL
    //   8: "-"                 -> DASH
    //   9: "3"                 -> INTEGER_LITERAL
    //  10: "%"                 -> PERCENTAGE
    //  11: "2"                 -> INTEGER_LITERAL
    //  12: "as"                -> AS
    //  13: "i32"               -> I32
    //  14: ";"                 -> SEMICOLON
    ASSERT_EQ(tokens.size(), 15,
              "There should be 15 tokens for the mixed operator input");
    ASSERT_EQ(tokens[0]->GetType(), TokenType::I32, "Token 0 should be I32");
    ASSERT_EQ(tokens[1]->GetType(), TokenType::IDENTIFIER,
              "Token 1 should be IDENTIFIER");
    ASSERT_EQ(tokens[2]->GetType(), TokenType::EQUAL,
              "Token 2 should be EQUAL");
    ASSERT_EQ(tokens[3]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 3 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[4]->GetType(), TokenType::ASTERISK,
              "Token 4 should be ASTERISK");
    ASSERT_EQ(tokens[5]->GetType(), TokenType::FLOAT_LITERAL,
              "Token 5 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[6]->GetType(), TokenType::PLUS, "Token 6 should be PLUS");
    ASSERT_EQ(tokens[7]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 7 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[8]->GetType(), TokenType::DASH, "Token 8 should be DASH");
    ASSERT_EQ(tokens[9]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 9 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[10]->GetType(), TokenType::PERCENTAGE,
              "Token 10 should be PERCENTAGE");
    ASSERT_EQ(tokens[11]->GetType(), TokenType::INTEGER_LITERAL,
              "Token 11 should be INTEGER_LITERAL");
    ASSERT_EQ(tokens[12]->GetType(), TokenType::AS, "Token 12 should be AS");
    ASSERT_EQ(tokens[13]->GetType(), TokenType::I32, "Token 13 should be I32");
    ASSERT_EQ(tokens[14]->GetType(), TokenType::SEMICOLON,
              "Token 14 should be SEMICOLON");
}
