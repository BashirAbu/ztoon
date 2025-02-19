#include "ztest.h"

#include "parser/parser.h"

TEST(ParserTest)
{
    std::string source = R"(;;)";
    Lexer lexer;
    lexer.Tokenize(source, "basic_binary_expr.ztoon");
    Parser parser(lexer.GetTokens());

    parser.Parse();

    parser.PrettyPrintAST();
}
