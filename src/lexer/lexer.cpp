#include "error_report.h"
#include "lexer.h"
#include "parser/parser.h"
#include "utils/memory_arean.h"
#include <cstdint>
#include <format>
#include <iostream>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>

MemoryArena gZtoonArena(ZTOON_ARENA_SIZE);
Stage currentStage;
uint32_t TokenDataTypeBitWidth(TokenType type)
{

    switch (type)
    {

    case TokenType::I8:
        return 8;
    case TokenType::I16:
        return 16;
    case TokenType::I32:
        return 32;
    case TokenType::I64:
        return 64;
    case TokenType::U8:
        return 8;
    case TokenType::U16:
        return 16;
    case TokenType::U32:
        return 32;
    case TokenType::U64:
        return 64;
    case TokenType::F32:
        return 32;
    case TokenType::F64:
        return 64;
    case TokenType::BOOL:
        return 1;
    default:
        return 0;
    }
    return 0;
}
std::string TokenDataTypeToString(TokenType type)
{
    switch (type)
    {

    case TokenType::I8:
        return "i8";
    case TokenType::I16:
        return "i16";
    case TokenType::I32:
        return "i32";
    case TokenType::I64:
        return "i64";
    case TokenType::U8:
        return "u8";
    case TokenType::U16:
        return "u16";
    case TokenType::U32:
        return "u32";
    case TokenType::U64:
        return "u64";
    case TokenType::F32:
        return "f32";
    case TokenType::F64:
        return "f64";
    case TokenType::BOOL:
        return "bool";
    case TokenType::NOTYPE:
        return "notype";
    default:
        return "Unknown type";
    }
    return "Unknown type";
}

bool IsLiteralToken(TokenType type)
{
    return TokenMatch(
        type, TokenType::FLOAT_LITERAL, TokenType::INTEGER_LITERAL,
        TokenType::INTEGER_LITERAL_HEX, TokenType::INTEGER_LITERAL_BINARY,
        TokenType::STRING_LITERAL, TokenType::CHARACTER_LITERAL,
        TokenType::FALSE, TokenType::TRUE, TokenType::RAW_STRING_LITERAL);
}

bool IsNumerical(TokenType type) { return IsInteger(type) || IsFloat(type); }
bool IsInteger(TokenType type)
{

    return TokenMatch(type, TokenType::I8, TokenType::I16, TokenType::I32,
                      TokenType::I64, TokenType::U8, TokenType::U16,
                      TokenType::U32, TokenType::U64);
}

bool IsSigned(TokenType type)
{
    return TokenMatch(type, TokenType::I8, TokenType::I16, TokenType::I32,
                      TokenType::I64);
}

bool IsFloat(TokenType type)
{
    return TokenMatch(type, TokenType::F32, TokenType::F64);
}

bool IsDataType(TokenType type)
{
    return TokenMatch(type, TokenType::I8, TokenType::I16, TokenType::I32,
                      TokenType::I64, TokenType::U8, TokenType::U16,
                      TokenType::U32, TokenType::U64, TokenType::F32,
                      TokenType::F64, TokenType::BOOL);
}
bool IsCompoundAssignment(TokenType type)
{
    return TokenMatch(type, TokenType::PLUS_EQUAL, TokenType::DASH_EQUAL,
                      TokenType::ASTERISK_EQUAL, TokenType::SLASH_EQUAL,
                      TokenType::PERCENTAGE_EQUAL, TokenType::BITWISE_AND_EQUAL,
                      TokenType::BITWISE_OR_EQUAL, TokenType::BITWISE_XOR_EQUAL,
                      TokenType::SHIFT_LEFT_EQUAL,
                      TokenType::SHIFT_RIGHT_EQUAL);
}
Lexer::Lexer()
{
    currentStage = Stage::LEXER;
    patterns.push_back({std::regex(R"(\r\n|\n|\r)"), TokenType::NEW_LINE});
    patterns.push_back({std::regex(R"(^[ \t]+)"), TokenType::WHITE_SPACE});
    // patterns.push_back({std::regex(R"(^/\*[\s\S]*?\*/)"),
    // TokenType::COMMENT});
    patterns.push_back(
        {std::regex(R"(//.*|/\*[\s\S]*?\*/)"), TokenType::COMMENT});
    patterns.push_back({std::regex(R"(^i8\b)"), TokenType::I8});
    patterns.push_back({std::regex(R"(^i16\b)"), TokenType::I16});
    patterns.push_back({std::regex(R"(^i32\b)"), TokenType::I32});
    patterns.push_back({std::regex(R"(^i64\b)"), TokenType::I64});
    patterns.push_back({std::regex(R"(^u8\b)"), TokenType::U8});
    patterns.push_back({std::regex(R"(^u16\b)"), TokenType::U16});
    patterns.push_back({std::regex(R"(^u32\b)"), TokenType::U32});
    patterns.push_back({std::regex(R"(^u64\b)"), TokenType::U64});
    patterns.push_back({std::regex(R"(^f32\b)"), TokenType::F32});
    patterns.push_back({std::regex(R"(^f64\b)"), TokenType::F64});
    patterns.push_back({std::regex(R"(^sizeof\b)"), TokenType::SIZEOF});
    patterns.push_back({std::regex(R"(^bool\b)"), TokenType::BOOL});
    patterns.push_back({std::regex(R"(^if\b)"), TokenType::IF});
    patterns.push_back({std::regex(R"(^else\b)"), TokenType::ELSE});
    patterns.push_back({std::regex(R"(^while\b)"), TokenType::WHILE});
    patterns.push_back({std::regex(R"(^switch\b)"), TokenType::SWITCH});
    patterns.push_back({std::regex(R"(^case\b)"), TokenType::CASE});
    patterns.push_back({std::regex(R"(^default\b)"), TokenType::DEFAULT});
    patterns.push_back({std::regex(R"(^for\b)"), TokenType::FOR});
    patterns.push_back({std::regex(R"(^fn\b)"), TokenType::FN});
    patterns.push_back({std::regex(R"(^ret\b)"), TokenType::RET});
    patterns.push_back({std::regex(R"(^readonly\b)"), TokenType::READONLY});
    patterns.push_back({std::regex(R"(^break\b)"), TokenType::BREAK});
    patterns.push_back({std::regex(R"(^continue\b)"), TokenType::CONTINUE});
    patterns.push_back({std::regex(R"(^nullptr\b)"), TokenType::NULL_PTR});
    patterns.push_back({std::regex(R"(^package\b)"), TokenType::PACKAGE});
    patterns.push_back({std::regex(R"(^import\b)"), TokenType::IMPORT});
    patterns.push_back({std::regex(R"(^pub\b)"), TokenType::PUB});
    patterns.push_back({std::regex(R"(^defer\b)"), TokenType::DEFER});

    patterns.push_back({std::regex(R"(^struct\b)"), TokenType::STRUCT});
    patterns.push_back({std::regex(R"(^union\b)"), TokenType::UNION});
    patterns.push_back({std::regex(R"(^enum\b)"), TokenType::ENUM});
    patterns.push_back({std::regex(R"(^\.\.\.)"), TokenType::VAR_ARGS});

    patterns.push_back({std::regex(R"(^<\[)"), TokenType::LEFT_ANGLE_SQUARE});
    patterns.push_back({std::regex(R"(^\]>)"), TokenType::RIGHT_SQUARE_ANGLE});
    patterns.push_back({std::regex(R"(^->)"), TokenType::ARROW});
    patterns.push_back({std::regex(R"(^--)"), TokenType::DASH_DASH});
    patterns.push_back({std::regex(R"(^\+=)"), TokenType::PLUS_EQUAL});
    patterns.push_back({std::regex(R"(^-=)"), TokenType::DASH_EQUAL});
    patterns.push_back({std::regex(R"(^\*=)"), TokenType::ASTERISK_EQUAL});
    patterns.push_back({std::regex(R"(^/=)"), TokenType::SLASH_EQUAL});
    patterns.push_back({std::regex(R"(^%=)"), TokenType::PERCENTAGE_EQUAL});
    patterns.push_back({std::regex(R"(^&=)"), TokenType::BITWISE_AND_EQUAL});
    patterns.push_back({std::regex(R"(^\^=)"), TokenType::BITWISE_XOR_EQUAL});
    patterns.push_back({std::regex(R"(^\|=)"), TokenType::BITWISE_OR_EQUAL});
    patterns.push_back({std::regex(R"(^<<=)"), TokenType::SHIFT_LEFT_EQUAL});
    patterns.push_back({std::regex(R"(^>>=)"), TokenType::SHIFT_RIGHT_EQUAL});
    patterns.push_back({std::regex(R"(^\|\|)"), TokenType::OR});
    patterns.push_back({std::regex(R"(^&&)"), TokenType::AND});
    patterns.push_back({std::regex(R"(^<<)"), TokenType::SHIFT_LEFT});
    patterns.push_back({std::regex(R"(^>>)"), TokenType::SHIFT_RIGHT});
    patterns.push_back({std::regex(R"(^<=)"), TokenType::LESS_EQUAL});
    patterns.push_back({std::regex(R"(^<)"), TokenType::LESS});
    patterns.push_back({std::regex(R"(^>=)"), TokenType::GREATER_EQUAL});
    patterns.push_back({std::regex(R"(^>)"), TokenType::GREATER});
    patterns.push_back({std::regex(R"(^==)"), TokenType::EQUAL_EQUAL});
    patterns.push_back({std::regex(R"(^!=)"), TokenType::EXCLAMATION_EQUAL});
    patterns.push_back({std::regex(R"(^&)"), TokenType::BITWISE_AND});
    patterns.push_back({std::regex(R"(^\|)"), TokenType::BITWISE_OR});
    patterns.push_back({std::regex(R"(^\^)"), TokenType::BITWISE_XOR});
    patterns.push_back({std::regex(R"(^\+\+)"), TokenType::PLUS_PLUS});
    patterns.push_back({std::regex(R"(^\*)"), TokenType::ASTERISK});
    patterns.push_back({std::regex(R"(^/)"), TokenType::SLASH});
    patterns.push_back({std::regex(R"(^\+)"), TokenType::PLUS});
    patterns.push_back({std::regex(R"(^-)"), TokenType::DASH});
    patterns.push_back({std::regex(R"(^\%)"), TokenType::PERCENTAGE});
    patterns.push_back({std::regex(R"(^as\b)"), TokenType::AS});
    patterns.push_back({std::regex(R"(^::)"), TokenType::DOUBLE_COLON});
    patterns.push_back({std::regex(R"(^:)"), TokenType::COLON});
    patterns.push_back({std::regex(R"(^;)"), TokenType::SEMICOLON});
    patterns.push_back({std::regex(R"(^\()"), TokenType::LEFT_PAREN});
    patterns.push_back({std::regex(R"(^\))"), TokenType::RIGHT_PAREN});
    patterns.push_back({std::regex(R"(^\{)"), TokenType::LEFT_CURLY_BRACKET});
    patterns.push_back({std::regex(R"(^\})"), TokenType::RIGHT_CURLY_BRACKET});
    patterns.push_back({std::regex(R"(^\[)"), TokenType::LEFT_SQUARE_BRACKET});
    patterns.push_back({std::regex(R"(^\])"), TokenType::RIGHT_SQUARE_BRACKET});
    patterns.push_back({std::regex(R"(^\.)"), TokenType::PERIOD});
    patterns.push_back({std::regex(R"(^=)"), TokenType::EQUAL});
    patterns.push_back({std::regex(R"(^!)"), TokenType::EXCLAMATION});
    patterns.push_back({std::regex(R"(^~)"), TokenType::TILDE});
    patterns.push_back({std::regex(R"(^,)"), TokenType::COMMA});
    patterns.push_back({std::regex(R"(^\?)"), TokenType::QUESTION_MARK});
    patterns.push_back({std::regex(R"(^@)"), TokenType::AT});

    patterns.push_back(
        {std::regex(R"(^0x([0-9a-fA-F]+))"), TokenType::INTEGER_LITERAL_HEX});
    patterns.push_back(
        {std::regex(R"(^0b([0-1]+))"), TokenType::INTEGER_LITERAL_BINARY});
    patterns.push_back(
        {std::regex(R"(^(\d+\.\d+)|(\.\d+))"), TokenType::FLOAT_LITERAL});
    patterns.push_back({std::regex(R"(^\d+)"), TokenType::INTEGER_LITERAL});

    patterns.push_back(
        {std::regex("^\"(.*?)(?:^|[^\\\\])\""), TokenType::STRING_LITERAL});
    patterns.push_back({std::regex("^R\"\\(([\\s\\S]*?)(?:^|[^\\\\])\\)\""),
                        TokenType::RAW_STRING_LITERAL});
    patterns.push_back({std::regex("^\'\\\\?.?(?:^|[^\\\\])\'"),
                        TokenType::CHARACTER_LITERAL});
    patterns.push_back({std::regex(R"(^false\b)"), TokenType::FALSE});
    patterns.push_back({std::regex(R"(^true\b)"), TokenType::TRUE});
    patterns.push_back(
        {std::regex(R"(^[_a-zA-Z][a-zA-Z0-9_]*)"), TokenType::IDENTIFIER});
}

Lexer::~Lexer() {}
void Lexer::Tokenize(std::string sourceCode, std::string filename)
{
    size_t pos = 0;
    size_t oldPos = 0;
    size_t colNumber = 1;
    size_t lineNumber = 1;
    while (pos < sourceCode.length())
    {
        bool matched = false;

        std::smatch match;

        std::string remaining = sourceCode.substr(pos);
        for (const Pattern &pattern : patterns)
        {
            if (std::regex_search(remaining, match, pattern.regex,
                                  std::regex_constants::match_continuous))
            {
                matched = true;
                if (pattern.type == TokenType::NEW_LINE)
                {
                    lineNumber++;
                    colNumber = 1;
                    oldPos = pos;
                    pos += match.length();
                    break;
                }

                Token *token = nullptr;

                if (pattern.type == TokenType::WHITE_SPACE ||
                    pattern.type == TokenType::COMMENT)
                {
                    oldPos = pos;
                    pos += match.length();
                    colNumber += pos - oldPos;
                    break;
                }

                if (IsLiteralToken(pattern.type))
                {
                    switch (pattern.type)
                    {
                    case TokenType::INTEGER_LITERAL:
                    {

                        token = gZtoonArena.Allocate<TokenLiteral<int32_t>>(
                            pattern.type, 0);
                        token->filename = filename;
                        token->lineNumber = lineNumber;
                        token->lexeme = match.str();
                        token->lineStr = token->lexeme;
                        uint64_t value;
                        try
                        {
                            value = std::stoull(match.str());
                        }
                        catch (const std::out_of_range &)
                        {
                            CodeErrString ces;
                            ces.firstToken = token;
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError("Integer literal is larger than 64bit",
                                        ces);
                        }

                        dynamic_cast<TokenLiteral<int32_t> *>(token)->value =
                            (int32_t)value;
                    }
                    break;
                    case TokenType::INTEGER_LITERAL_HEX:
                    {
                        size_t numberOfDigits = match.str().length();
                        bool error = numberOfDigits > 18;

                        bool is32Bits = numberOfDigits <= 10 ? true : false;

                        if (is32Bits)
                        {
                            token =
                                gZtoonArena.Allocate<TokenLiteral<uint32_t>>(
                                    TokenType::INTEGER_LITERAL, 0);
                        }
                        else
                        {
                            token =
                                gZtoonArena.Allocate<TokenLiteral<uint64_t>>(
                                    TokenType::INTEGER_LITERAL, 0);
                        }
                        token->filename = filename;
                        token->lineNumber = lineNumber;
                        token->lexeme = match.str();
                        token->lineStr = token->lexeme;
                        if (error)
                        {
                            CodeErrString ces;
                            ces.firstToken = token;
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError("Integer literal is larger than 64bit",
                                        ces);
                        }
                        std::stringstream ss;
                        ss << std::hex << match.str();
                        uint64_t hexValue = 0;
                        ss >> hexValue;

                        reinterpret_cast<TokenLiteral<uint64_t> *>(token)
                            ->value = hexValue;
                    }
                    break;
                    case TokenType::INTEGER_LITERAL_BINARY:
                    {
                        std::string literalString = match.str();
                        literalString.erase(literalString.begin());
                        literalString.erase(literalString.begin());
                        size_t numberOfDigits = literalString.length();
                        bool error = numberOfDigits > 64;

                        bool is32Bits = numberOfDigits <= 32 ? true : false;

                        if (is32Bits)
                        {
                            token =
                                gZtoonArena.Allocate<TokenLiteral<uint32_t>>(
                                    TokenType::INTEGER_LITERAL, 0);
                        }
                        else
                        {
                            token =
                                gZtoonArena.Allocate<TokenLiteral<uint64_t>>(
                                    TokenType::INTEGER_LITERAL, 0);
                        }
                        token->filename = filename;
                        token->lineNumber = lineNumber;
                        token->lexeme = match.str();
                        token->lineStr = token->lexeme;
                        if (numberOfDigits > 64)
                        {
                            CodeErrString ces;
                            ces.firstToken = token;
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError("Integer literal is larger than 64bit",
                                        ces);
                        }
                        uint64_t binaryValue =
                            std::stoull(literalString, nullptr, 2);
                        reinterpret_cast<TokenLiteral<uint64_t> *>(token)
                            ->value = binaryValue;
                    }
                    break;
                    case TokenType::FLOAT_LITERAL:
                    {

                        token = gZtoonArena.Allocate<TokenLiteral<float>>(
                            pattern.type, 0);
                        token->filename = filename;
                        token->lineNumber = lineNumber;
                        token->lexeme = match.str();
                        token->lineStr = token->lexeme;
                        double value;
                        try
                        {
                            value = std::stod(match.str());
                        }
                        catch (const std::out_of_range &)
                        {
                            CodeErrString ces;
                            ces.firstToken = token;
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError("float literal is larger than 64bit",
                                        ces);
                        }

                        dynamic_cast<TokenLiteral<float> *>(token)->value =
                            (float)value;
                    }

                    break;
                    case TokenType::CHARACTER_LITERAL:
                    {
                        std::string literal = match.str();

                        literal.pop_back();
                        literal.erase(literal.begin());
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\n"), "\n");

                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\t"), "\t");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\r"), "\r");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\b"), "\b");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\f"), "\f");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\a"), "\a");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\v"), "\v");
                        literal = std::regex_replace(
                            literal, std::regex("\\\\\""), "\"");
                        literal = std::regex_replace(literal,
                                                     std::regex("\\\\'"), "\'");
                        literal = std::regex_replace(
                            literal, std::regex("\\\\\\\\"), "\\");
                        char character = *literal.begin();
                        if (!literal.empty())
                        {
                            literal = std::regex_replace(
                                literal, std::regex("\\\\0"), "\0");
                            if (literal.empty())
                            {
                                character = '\0';
                            }
                        }
                        token = gZtoonArena.Allocate<TokenLiteral<int8_t>>(
                            pattern.type, character);
                    }
                    break;
                    case TokenType::STRING_LITERAL:
                    {
                        std::string str = match.str();
                        str.pop_back();
                        str.erase(str.begin());
                        str =
                            std::regex_replace(str, std::regex("\\\\n"), "\n");
                        str =
                            std::regex_replace(str, std::regex("\\\\0"), "\0");
                        str =
                            std::regex_replace(str, std::regex("\\\\t"), "\t");
                        str =
                            std::regex_replace(str, std::regex("\\\\r"), "\r");
                        str =
                            std::regex_replace(str, std::regex("\\\\b"), "\b");
                        str =
                            std::regex_replace(str, std::regex("\\\\f"), "\f");
                        str =
                            std::regex_replace(str, std::regex("\\\\a"), "\a");
                        str =
                            std::regex_replace(str, std::regex("\\\\v"), "\v");
                        str =
                            std::regex_replace(str, std::regex("\\\\\""), "\"");
                        str =
                            std::regex_replace(str, std::regex("\\\\'"), "\'");
                        str = std::regex_replace(str, std::regex("\\\\\\\\"),
                                                 "\\");
                        token = gZtoonArena.Allocate<TokenLiteral<std::string>>(
                            pattern.type, str);
                    }
                    break;
                    case TokenType::RAW_STRING_LITERAL:
                    {
                        std::string str = match.str();
                        str.pop_back();
                        str.pop_back();
                        str.erase(str.begin());
                        str.erase(str.begin());
                        str.erase(str.begin());
                        token = gZtoonArena.Allocate<TokenLiteral<std::string>>(
                            TokenType::STRING_LITERAL, str);
                    }
                    break;
                    case TokenType::TRUE:
                        token = gZtoonArena.Allocate<TokenLiteral<bool>>(
                            pattern.type, true);
                        break;
                    case TokenType::FALSE:
                        token = gZtoonArena.Allocate<TokenLiteral<bool>>(
                            pattern.type, false);
                        break;

                    default:
                        break;
                    }
                }

                if (!token)
                {
                    token = gZtoonArena.Allocate<Token>(pattern.type);
                }

                token->filename = filename;
                token->lineNumber = lineNumber;
                token->colNumber = colNumber;
                token->lexeme = match.str();

                tokens.push_back(token);

                if (pattern.type == TokenType::UNKNOWN)
                {
                    token = gZtoonArena.Allocate<Token>(TokenType::UNKNOWN);
                    CodeErrString ces = {};
                    ces.firstToken = token;
                    ces.str = token->lexeme;
                    ReportError("Unknown token.", ces);
                }
                oldPos = pos;
                pos += match.length();

                colNumber += pos - oldPos;
                break;
            }
        }

        if (!matched)
        {
            CodeErrString ces = {};
            ReportError("Unkown token.", ces);
        }
    }

    Token *endOfFileToken = gZtoonArena.Allocate<Token>(TokenType::END_OF_FILE);
    tokens.push_back(endOfFileToken);

    // iterate over each line and match tokens with their line.
    std::istringstream sourceSS(sourceCode);
    std::string lineStr = "";
    std::unordered_map<size_t, std::string> codeLinesMap;
    size_t index = 1;
    while (std::getline(sourceSS, lineStr))
    {
        codeLinesMap[index] = lineStr;
        index++;
    }

    for (Token *token : tokens)
    {
        token->lineStr = codeLinesMap[token->lineNumber];
    }
}

void Lexer::DebugPrint()
{
    size_t index = 0;
    for (Token *t : tokens)
    {
        std::cout << std::format("No.: {}, Token: {}, Line: {}, Col: {},  "
                                 "File: {}, Code Line: {}\n",
                                 index, t->GetLexeme(), t->GetLineNumber(),
                                 t->colNumber, t->filename, t->lineStr);
        index++;
    }
}
