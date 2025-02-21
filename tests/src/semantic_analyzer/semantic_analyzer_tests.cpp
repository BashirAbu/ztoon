#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

TEST(SemanticTesting)
{
    std::string source = "a:u32;a = 12;";
    Lexer lexer;
    lexer.Tokenize(source, "semantics.ztoon");
    Parser parser(lexer.GetTokens());
    auto statements = parser.Parse();
    parser.PrettyPrintAST();
    SemanticAnalyzer analyzer(statements);
    analyzer.Analize();

    parser.PrettyPrintAST();
}
