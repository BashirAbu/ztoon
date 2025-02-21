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
        ff : f32 = f as f32;
        big: u64 = (f as i32 * 12) as u64 - ( 122 || 232 + 1.0 as i32 ) as u64;
    )";
    Lexer lexer;
    lexer.Tokenize(source, "semantics.ztoon");
    Parser parser(lexer.GetTokens());
    auto statements = parser.Parse();
    parser.PrettyPrintAST();
    SemanticAnalyzer analyzer(statements);
    analyzer.Analize();

    parser.PrettyPrintAST();
}
