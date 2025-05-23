#pragma once
#include <cstdio>
#include <filesystem>
#include <vector>
#include <yaml-cpp/yaml.h>
#include <format>
void PrintError(std::string err);
void PrintMSG(std::string msg);
class ArgTokenizer
{
  public:
    enum class TokenType
    {
        INVALID,
        NEW,
        BUILD,
        PROJECT,
        DEBUG,
        RELEASE,
        IR, // for intermediate representation
        V,  // for verbose
        Q,  // quiet
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

    std::string buildThisProject = "";
    bool verbose = false;
    bool printIR = false;
    bool quiet = false;

  private:
    size_t currentIndex = 0;
    ArgTokenizer *argTokenizer = nullptr;
};

struct Project
{
    enum class Platform
    {
        NONE,
        WINDOWS,
        LINUX,
        MACOS,
        IOS,
        ANDROID,
        WASM,

    };
    Platform platform = Platform::NONE;
    enum class Type
    {
        ZLIB,
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
        else if (typeStr == "zlib")
        {
            return Type::ZLIB;
        }
        else if (typeStr == "exe")
        {
            return Type::EXE;
        }
        else
        {
            PrintError(std::format("Unknown binary type '{}'", typeStr));
        }
        return (Type)0;
    }
    static std::string PrjectTypeToStr(Type type)
    {

        switch (type)
        {
        case Type::EXE:
        {
            return "Executable";
        }
        break;
        case Type::STATIC_LIB:
        {
            return "Static Library";
        }
        break;
        case Type::SHARED_LIB:
        {
            return "Shared Library";
        }
        break;
        case Type::ZLIB:
        {
            return "Ztoon Library";
        }
        break;
        default:
        {
            return "Unknown";
        }
        }
    }
    std::string name;
    Type type = Type::EXE;
    bool debugBuild = true;

    enum class OptLevel
    {
        O0,
        O1,
        O2,
        O3,
        OS,
        OZ
    };

    OptLevel StrToOptLevel(std::string level)
    {
        if (level == "o0")
            return OptLevel::O0;
        else if (level == "o1")
            return OptLevel::O1;
        else if (level == "o2")
            return OptLevel::O2;
        else if (level == "o3")
            return OptLevel::O3;
        else if (level == "os")
            return OptLevel::OS;
        else if (level == "oz")
            return OptLevel::OZ;
        else
            PrintError(std::format("Unknown optimzation level '{}'", level));

        return (OptLevel)0;
    }
    std::string OptLevelToStr(OptLevel level)
    {
        switch (level)
        {
        case OptLevel::O0:
        {
            return "o0";
        }
        break;
        case OptLevel::O1:
        {
            return "o1";
        }
        break;
        case OptLevel::O2:
        {
            return "o2";
        }
        break;
        case OptLevel::O3:
        {
            return "o3";
        }
        break;
        case OptLevel::OZ:
        {
            return "oz";
        }
        break;
        case OptLevel::OS:
        {
            return "os";
        }
        break;
        }
    }

    struct CommonFlags
    {
    };
    CommonFlags commonFlags;
    struct DebugFlags
    {
        OptLevel optLevel = OptLevel::O0;
    };
    DebugFlags debugFlags;

    struct ReleaseFlags
    {
        OptLevel optLevel = OptLevel::O3;
    };
    ReleaseFlags releaseFlags;
    struct LinkerFlags
    {
        enum class Type
        {
            CONSOLE,
            WINDOW,
        };
        Type type = Type::CONSOLE;
        void SetType(std::string strType)
        {
            if (strType == "console")
            {
                type = Type::CONSOLE;
            }
            else if (strType == "window")
            {
                type = Type::WINDOW;
            }
            else
            {
                PrintError(
                    std::format("Unknown executable type '{}'", strType));
            }
        }
        bool noCRT = false;
        enum class CRT_LinkType
        {
            STATIC,
            DYNAMIC,
        };
        void SetCRTLinkType(std::string linkType)
        {
            if (linkType == "static")
            {
                crtLinkType = CRT_LinkType::STATIC;
            }
            else if (linkType == "dynamic")
            {
                crtLinkType = CRT_LinkType::DYNAMIC;
            }
            else
            {
                PrintError(std::format("Unknown link type '{}'", linkType));
            }
        }
        CRT_LinkType crtLinkType = CRT_LinkType::STATIC;
        std::string entry = "";

        struct NativeLib
        {
            std::string name;
            std::filesystem::path relative_path;
            Project::Type type;
        };

        std::vector<NativeLib> nativeLibs;
    };
    LinkerFlags linkerFlags;

    struct Dependency
    {
        std::string name;
        std::filesystem::path relativePath;
        Project *project;
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

    bool printIRcode = false;
    bool verbose = false;
    bool quiet = false;
    void BuildProject(Project &project);
    void BuildWorkSpace();
    Project ParseProject(std::string projectName,
                         std::filesystem::path projectRelativePathToWorkSpace,
                         YAML::Node &projectRoot);
};
