#pragma once
#include "utils/memory_arean.h"
#include <string>
#include <type_traits>
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
    EQUAL
};

class Token
{
  public:
    Token(TokenType type) : type(type) {}
    virtual ~Token() {}
    TokenType GetType() { return type; }
    std::string GetLexeme() { return lexeme; }

  protected:
    TokenType type;
    std::string lexeme;
};

template <typename T>

class TokenLiteral : public Token
{
  public:
    TokenLiteral(TokenType type, T value) : Token(type), value(value) {}
    ~TokenLiteral() {}
    T GetValue() { return value; }

  private:
    T value = {};
};

class Lexer
{
  public:
    Lexer(std::string sourceCode);
    ~Lexer();

  private:
    std::vector<Token *> tokens;
};
