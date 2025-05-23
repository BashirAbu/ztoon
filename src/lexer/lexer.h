#pragma once
#include "utils/memory_arean.h"
#include <filesystem>
#include <regex>
#include <string>
#include <vector>
// One Megabyte
#define ZTOON_ARENA_SIZE 1024ull * 1024ull * 100ull
extern MemoryArena gZtoonArena;

enum class Stage
{
    LEXER,
    PARSER,
    SEMANTIC_ANALYZER,
    CODE_GEN
};
extern Stage currentStage;
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
    NOTYPE,
    NULL_PTR,
    INTEGER_LITERAL,
    INTEGER_LITERAL_HEX,
    INTEGER_LITERAL_BINARY,
    FLOAT_LITERAL,
    STRING_LITERAL,
    RAW_STRING_LITERAL,
    CHARACTER_LITERAL,
    TRUE,
    FALSE,
    QUESTION_MARK,
    COLON,
    DOUBLE_COLON,
    SEMICOLON,
    ARROW,
    IDENTIFIER,
    EQUAL,
    COMMA,
    PERIOD,

    PLUS_EQUAL,
    DASH_EQUAL,
    ASTERISK_EQUAL,
    SLASH_EQUAL,
    PERCENTAGE_EQUAL,
    BITWISE_AND_EQUAL,
    BITWISE_XOR_EQUAL,
    BITWISE_OR_EQUAL,
    SHIFT_LEFT_EQUAL,
    SHIFT_RIGHT_EQUAL,

    ASTERISK,
    SLASH,
    PLUS,
    PLUS_PLUS,

    OR,
    AND,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    EQUAL_EQUAL,
    EXCLAMATION_EQUAL,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    BITWISE_OR,
    BITWISE_AND,
    BITWISE_XOR,
    DASH,
    DASH_DASH,
    PERCENTAGE,
    TILDE,
    EXCLAMATION,
    AS,
    SIZEOF,
    END_OF_FILE,
    END_OF_PROGRAM,
    LEFT_PAREN,
    RIGHT_PAREN,
    DOT,
    AT,
    VAR_ARGS,

    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,
    LEFT_ANGLE_SQUARE,
    RIGHT_SQUARE_ANGLE,

    IF,
    ELSE,
    WHILE,
    FOR,
    FN,
    RET,
    READONLY,
    BREAK,
    CONTINUE,
    STRUCT,
    UNION,
    ENUM,
    SWITCH,
    CASE,
    DEFAULT,
    PACKAGE,
    IMPORT,
    PUB,
    DEFER,
};

std::string TokenDataTypeToString(TokenType type);

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
bool IsNumerical(TokenType type);
bool IsInteger(TokenType type);
bool IsSigned(TokenType type);
bool IsFloat(TokenType type);
bool IsCompoundAssignment(TokenType type);
bool IsDataType(TokenType type);
uint32_t TokenDataTypeBitWidth(TokenType type);
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
    std::filesystem::path GetFilepath() const { return filepath; }

  protected:
    TokenType type;
    std::string lexeme;
    std::string filename = "";
    std::filesystem::path filepath;
    size_t lineNumber = 0;
    size_t colNumber = 0;
    std::string lineStr = "";
    friend class Lexer;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class CodeGen;
    friend class Scope;
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
    friend class Lexer;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class Lexer
{
  public:
    Lexer();
    ~Lexer();
    void Tokenize(std::string sourceCode, std::filesystem::path filepath);
    void DebugPrint();
    std::vector<Token *> &GetTokens() { return tokens; }
    void EndProgram()
    {
        Token *token = gZtoonArena.Allocate<Token>(TokenType::END_OF_PROGRAM);
        tokens.push_back(token);
    }

  private:
    struct Pattern
    {
        std::regex regex;
        TokenType type;
    };
    std::vector<Pattern> patterns;
    std::vector<Token *> tokens;
};
