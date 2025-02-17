#include "lexer.h"
#include "utils/memory_arean.h"
#include <regex>
MemoryArena lexerArena(LEXER_ARENA_SIZE);

Lexer::Lexer(std::string sourceCode)
{
    struct Pattern
    {
        std::regex regex;
        TokenType type;
    };
    std::vector<Pattern> patterns = {
        {std::regex(R"(^\s+)"), TokenType::WHITE_SPACE},
        {std::regex(R"(^/\*[\s\S]*?\*/)"), TokenType::COMMENT},
        {std::regex(R"(^//.*[^\n])"), TokenType::COMMENT},
        {std::regex(R"(^i8\b)"), TokenType::I8},
        {std::regex(R"(^i16\b)"), TokenType::I16},
        {std::regex(R"(^i32\b)"), TokenType::I32},
        {std::regex(R"(^i64\b)"), TokenType::I64},
        {std::regex(R"(^u8\b)"), TokenType::U8},
        {std::regex(R"(^u16\b)"), TokenType::U16},
        {std::regex(R"(^u32\b)"), TokenType::U32},
        {std::regex(R"(^u64\b)"), TokenType::U64},
        {std::regex(R"(^f32\b)"), TokenType::F32},
        {std::regex(R"(^f64\b)"), TokenType::F64},
        {std::regex(R"(^bool\b)"), TokenType::BOOL},
    };
}

Lexer::~Lexer() {}
