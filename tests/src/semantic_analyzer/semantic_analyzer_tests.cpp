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
        else if false || 1 == 1
            b--;
        else {
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
