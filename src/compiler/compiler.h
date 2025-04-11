#pragma once
#include <filesystem>
#include <vector>

void PrintError(std::string err);
class ArgTokenizer
{
  public:
    enum class TokenType
    {
        INVALID,
        NEW,
        BUILD,
        DEBUG,
        RELEASE,
        IDENTIFIER,
        EOA,
    };
    struct Token
    {
        TokenType type;
        std::string arg;
    };

    void Tokenize(std::vector<std::string> args);

    std::vector<Token *> tokens;
};

class ArgParser
{
  public:
    ArgParser(std::vector<std::string> args);
    void Parse();
    void Advance();
    ArgTokenizer::Token *Prev();
    ArgTokenizer::Token *Peek();
    bool Consume(ArgTokenizer::TokenType type);

    std::string workSpaceName;

    ArgTokenizer::TokenType buildType = ArgTokenizer::TokenType::INVALID;

  private:
    size_t currentIndex = 0;
    ArgTokenizer *argTokenizer = nullptr;
};

struct Project
{
    enum class Type
    {
        NONE,
        STATIC_LIB,
        SHARED_LIB,
        EXE
    };
    static Type StrToPrjectType(std::string typeStr)
    {
        if (typeStr == "static-lib")
        {
            return Type::STATIC_LIB;
        }
        else if (typeStr == "shared-lib")
        {
            return Type::SHARED_LIB;
        }
        else if (typeStr == "exe")
        {
            return Type::EXE;
        }

        return Type::NONE;
    }
    std::string name;
    Type type = Type::NONE;
    std::vector<std::string> common_compiler_flags;
    std::vector<std::string> debug_compiler_flags;
    std::vector<std::string> release_compiler_flags;
    std::vector<std::string> common_linker_flags;
    std::vector<std::string> debug_linker_flags;
    std::vector<std::string> release_linker_flags;

    struct Dependency
    {
        std::string name;
        std::filesystem::path relativePath;
    };

    std::vector<Dependency> deps;

    std::filesystem::path relativePathToWorkSpace;

    std::string targetArch;
};

struct WorkSpace
{
    std::string name;
    struct ProjectBasicInfo
    {
        std::string name;
        std::filesystem::path relativePath;
        Project::Type type;
    };
    std::vector<ProjectBasicInfo> projectsInfo;
    std::vector<Project> projects;
};

class Compiler
{
  public:
    Compiler(int argv, char **argc);
    std::vector<std::string> args;
    ArgParser *argParser = nullptr;
    WorkSpace workSpace;

    void BuildWorkSpace();
};
