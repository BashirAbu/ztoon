#pragma once
#include "utils/memory_arean.h"
#include <regex>
#include <string>
#include <vector>
// One Megabyte
#define LEXER_ARENA_SIZE 1024ull * 1024ull
extern MemoryArena lexerArena;

#define SEPARATE_ARGS(...) SEPARATE_ARGS_IMPL(__VA_ARGS__)
#define SEPARATE_ARGS_IMPL(first, ...) first, ##__VA_ARGS__

#define MACRO_ENUM(enum_name, ...)                                             \
    enum class enum_name                                                       \
    {                                                                          \
        SEPARATE_ARGS(__VA_ARGS__)                                             \
    }

enum class TokenType
{
    UNKNOWN = 0,

    WHITE_SPACE,
    COMMENT,
    NEW_LINE,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    BOOL,

    INTEGER_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    CHARACTER_LITERAL,
    TRUE,
    FALSE,

    COLON,
    SEMICOLON,

    IDENTIFIER,
    EQUAL,

    ASTERISK,
    SLASH,
    PLUS,
    DASH,
    PERCENTAGE,
    AS,
};
template <typename... Types>

bool TokenMatch(TokenType type, Types... types)
{
    for (auto tokenType : {types...})
    {
        if (type == tokenType)
        {
            return true;
        }
    }

    return false;
}

bool IsLiteralToken(TokenType type);

bool IsInteger(TokenType type);
bool IsSigned(TokenType type);
bool IsFloat(TokenType type);
class Token
{
  public:
    Token(TokenType type) : type(type) {}
    virtual ~Token() {}
    TokenType GetType() const { return type; }
    std::string GetLexeme() const { return lexeme; }
    std::string GetFilename() const { return filename; }
    size_t GetLineNumber() const { return lineNumber; }
    size_t GetColNumber() const { return colNumber; }
    std::string GetLineStr() const { return lineStr; }

  protected:
    TokenType type;
    std::string lexeme;
    std::string filename = "";
    size_t lineNumber = 0;
    size_t colNumber = 0;
    std::string lineStr = "";
    friend class Lexer;
};

template <typename T>

class TokenLiteral : public Token
{
  public:
    TokenLiteral(TokenType type, T value) : Token(type), value(value) {}
    ~TokenLiteral() {}
    T GetValue() const { return value; }

  private:
    T value = {};
};

class Lexer
{
  public:
    Lexer();
    ~Lexer();
    void Tokenize(std::string sourceCode, std::string filename);
    void DebugPrint();
    const std::vector<Token *> &GetTokens() { return tokens; }

  private:
    struct Pattern
    {
        std::regex regex;
        TokenType type;
    };
    std::vector<Pattern> patterns;
    std::vector<Token *> tokens;
};
