#include "code_gen/code_gen.h"
#include "compiler.h"
#include "lexer/lexer.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include <cstdio>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <vector>
#include <yaml-cpp/yaml.h>
std::string basicZtoonExeMainFile = R"(
fn main() -> i32
{
    ret 0;
}
)";

std::string basicZtoonLibraryFile = R"(
fn add(a: i32, b: i32) -> i32
{
    ret a + b;
}
)";
Compiler::Compiler(int argv, char **argc)
{
    for (size_t i = 1; i < argv; i++)
    {
        args.push_back(argc[i]);
    }
    // end of args
    args.push_back("--eoa");
    argParser = gZtoonArena.Allocate<ArgParser>(args);
    argParser->Parse();

    if (!argParser->workSpaceName.empty())
    {
        if (std::filesystem::exists(argParser->workSpaceName) &&
            std::filesystem::is_directory(argParser->workSpaceName))
        {
            PrintError(std::format("Directory with name '{}' already exist.",
                                   argParser->workSpaceName));
        }

        std::filesystem::create_directory(argParser->workSpaceName);
        std::vector<std::filesystem::path> dirs = {

            argParser->workSpaceName + "/bin"};

        for (auto dir : dirs)
        {
            std::filesystem::create_directory(dir);
        }

        YAML::Node workSpaceRoot;

        workSpaceRoot["workspace"]["name"] = argParser->workSpaceName;

        std::string filename = argParser->workSpaceName + "/ztoon.yaml";

        std::ofstream workSpaceFile(filename);
        workSpaceFile << workSpaceRoot;
        workSpaceFile.close();
    }

    else if (argParser->buildType != ArgTokenizer::TokenType::INVALID)
    {
        std::fstream workspaceFile("ztoon.yaml");
        if (!workspaceFile.is_open())
        {
            PrintError("workspace's 'ztoon.yaml' file is missing");
        }

        YAML::Node workSpaceRoot;
        try
        {
            workSpaceRoot = YAML::Load(workspaceFile);
        }
        catch (const YAML::Exception &e)
        {
            std::cerr << "Error parsing workspace's 'ztoon.yaml' file: "
                      << e.what() << "\n";
        }
        if (workSpaceRoot["workspace"].IsDefined())
        {
            if (workSpaceRoot["workspace"]["name"].IsDefined())
            {
                workSpace.name =
                    workSpaceRoot["workspace"]["name"].as<std::string>();
            }
            else
            {

                PrintError("workspace's 'name' key is not found");
            }
        }
        else
        {
            PrintError("'workspace' key is not found");
        }

        if (workSpaceRoot["workspace"]["projects"].IsDefined())
        {
            if (!workSpaceRoot["workspace"]["projects"].IsSequence())
            {
                PrintError("workspace's 'projects' key is not a sequence.");
            }
        }
        else
        {
            PrintError("workspace's 'projects' key is not found");
        }

        for (auto projMap : workSpaceRoot["workspace"]["projects"])
        {
            auto proj = projMap.begin();
            WorkSpace::ProjectBasicInfo info;
            info.name = proj->first.as<std::string>();
            if (!proj->second["relative_path"].IsDefined())
            {
                PrintError(std::format("{}'s 'relative_path' key is missing",
                                       info.name));
            }
            info.relativePath = proj->second["relative_path"].as<std::string>();
            if (info.relativePath.empty())
            {
                PrintError(std::format(
                    "{}'s 'relative_path' value cannot be empty", info.name));
            }
            if (!proj->second["type"].IsDefined())
            {
                PrintError(
                    std::format("{}'s 'type' key is missing", info.name));
            }
            info.type = Project::StrToPrjectType(
                proj->second["type"].as<std::string>());
            workSpace.projectsInfo.push_back(info);
        }

        // check if projects exist or not.
        for (auto proj : workSpace.projectsInfo)
        {
            if (std::filesystem::exists(proj.relativePath) &&
                std::filesystem::is_directory(proj.relativePath))
            {
                std::fstream projectFile(proj.relativePath / "ztoon.yaml");
                if (!projectFile.is_open())
                {
                    PrintError(std::format("{}'s 'ztoon.yaml' file is missing",
                                           proj.name));
                }

                YAML::Node projectRoot;

                try
                {
                    projectRoot = YAML::Load(projectFile);
                }
                catch (const YAML::Exception &e)
                {
                    std::cerr
                        << std::format("Error parsing {}'s 'ztoon.yaml' file: ",
                                       proj.name)
                        << e.what() << "\n";
                }

                Project project;
                project.debugBuild =
                    argParser->buildType == ArgTokenizer::TokenType::DEBUG;
                project.relativePathToWorkSpace = proj.relativePath;
                if (!projectRoot[proj.name].IsDefined())
                {
                    PrintError(std::format("'{}' key is not found", proj.name));
                }
                project.name = projectRoot.begin()->first.as<std::string>();
                if (!projectRoot[proj.name]["type"].IsDefined())
                {
                    PrintError(
                        std::format("{}'s 'type' key is not found", proj.name));
                }
                project.type = Project::StrToPrjectType(
                    projectRoot[proj.name]["type"].as<std::string>());

                if (projectRoot[proj.name]["common_flags"])
                {
                }

                if (projectRoot[proj.name]["debug_flags"])
                {
                    if (projectRoot[proj.name]["debug_flags"]["opt_level"]
                            .IsDefined())
                    {
                        project.debugFlags.optLevel = project.StrToOptLevel(
                            projectRoot[proj.name]["debug_flags"]["opt_level"]
                                .as<std::string>());
                    }
                }
                if (projectRoot[proj.name]["release_flags"])
                {
                    if (projectRoot[proj.name]["release_flags"]["opt_level"]
                            .IsDefined())
                    {
                        project.releaseFlags.optLevel = project.StrToOptLevel(
                            projectRoot[proj.name]["release_flags"]["opt_level"]
                                .as<std::string>());
                    }
                }
                if (projectRoot[proj.name]["linker_flags"])
                {

                    if (projectRoot[proj.name]["linker_flags"]["exe_type"]
                            .IsDefined())
                    {
                        project.linkerFlags.SetType(
                            projectRoot[proj.name]["linker_flags"]["exe_type"]
                                .as<std::string>());
                    }

                    if (projectRoot[proj.name]["linker_flags"]["no_crt"]
                            .IsDefined())
                    {
                        project.linkerFlags.noCRT =
                            projectRoot[proj.name]["linker_flags"]["no_crt"]
                                .as<bool>();
                    }
                    if (projectRoot[proj.name]["linker_flags"]["crt_link_type"]
                            .IsDefined())
                    {
                        project.linkerFlags.SetCRTLinkType(
                            projectRoot[proj.name]["linker_flags"]
                                       ["crt_link_type"]
                                           .as<std::string>());
                    }
                    if (projectRoot[proj.name]["linker_flags"]["entry"]
                            .IsDefined())
                    {
                        project.linkerFlags.entry =
                            projectRoot[proj.name]["linker_flags"]["entry"]
                                .as<std::string>();
                    }
                }

                if (projectRoot[proj.name]["deps"])
                {
                    if (!projectRoot[proj.name]["deps"].IsSequence())
                    {
                        PrintError(std::format("{}'s 'deps' "
                                               "key must be a sequence type",
                                               proj.name));
                    }

                    for (auto dep : projectRoot[proj.name]["deps"])
                    {
                        auto depMap = dep.begin();
                        if (!depMap->first.IsDefined())
                        {
                            PrintError(std::format(
                                "Missing key in '{}'s deps sequence",
                                proj.name));
                        }
                        Project::Dependency dependency;
                        dependency.name = depMap->first.as<std::string>();
                        if (!depMap->second["relative_path"].IsDefined())
                        {
                            PrintError(std::format(
                                "'{}' key in project {}'s deps is missing",
                                dependency.name, proj.name));
                        }
                        dependency.relativePath =
                            depMap->second["relative_path"].as<std::string>();
                    }
                }
                if (projectRoot[proj.name]["arch"].IsDefined())
                {
                    project.targetArch =
                        projectRoot[proj.name]["arch"].as<std::string>();
                }
                else
                {
                    // TODO remove
                    project.targetArch = "x86_64-pc-windows-msvc";
                }

                workSpace.projects.push_back(project);
            }
            else
            {
                // create project
                std::filesystem::create_directories(proj.relativePath);
                std::filesystem::create_directory(proj.relativePath / "src");
                std::filesystem::create_directory(proj.relativePath / "deps");
                YAML::Node projNode;
                std::string type = "none";
                switch (proj.type)
                {
                case Project::Type::EXE:
                {
                    type = "exe";
                    std::ofstream defFile(proj.relativePath / "src" /
                                          "main.ztoon");
                    defFile << basicZtoonExeMainFile;
                    defFile.close();
                }
                break;
                case Project::Type::STATIC_LIB:
                {
                    type = "static-lib";
                    std::ofstream defFile(proj.relativePath / "src" /
                                          "lib.ztoon");
                    defFile << basicZtoonLibraryFile;
                    defFile.close();
                }
                break;
                case Project::Type::SHARED_LIB:
                {
                    type = "shared-lib";
                    std::ofstream defFile(proj.relativePath / "src" /
                                          "lib.ztoon");
                    defFile << basicZtoonLibraryFile;
                    defFile.close();
                }
                break;
                default:
                {
                    type = "none";
                }
                break;
                }
                projNode[proj.name]["type"] = type;
                std::ofstream projectFile(proj.relativePath / "ztoon.yaml");
                projectFile << projNode;
                projectFile.close();
            }
        }

        BuildWorkSpace();
    }
    else
    {
        std::cout << "Hi from ztoon compiler. TODO: add -help section\n";
    }
}

void Compiler::BuildWorkSpace()
{
    // go over each project
    for (auto proj : workSpace.projects)
    {
        auto srcPath = proj.relativePathToWorkSpace / "src";
        if (std::filesystem::exists(srcPath) &&
            std::filesystem::is_directory(srcPath))
        {
            Lexer lexer;
            for (auto sourceFile :
                 std::filesystem::recursive_directory_iterator(srcPath))
            {
                if (sourceFile.path().extension() == ".ztoon")
                {
                    std::fstream srcFile(sourceFile.path());
                    if (!srcFile.is_open())
                    {
                        PrintError(
                            std::format("Failed to open file '{}'",
                                        sourceFile.path().generic_string()));
                    }

                    std::stringstream ss;
                    ss << srcFile.rdbuf();
                    std::string content = ss.str();
                    std::string filename =
                        sourceFile.path().filename().generic_string();
                    lexer.Tokenize(content, filename);
                }
            }

            Parser parser(lexer.GetTokens());
            auto stmts = parser.Parse();
            SemanticAnalyzer semanticAnalyzer(stmts);
            semanticAnalyzer.Analize();
            CodeGen codeGen(semanticAnalyzer, proj.targetArch);
            codeGen.GenIR();
            codeGen.Compile(proj);
        }
        else
        {
            PrintError(std::format("Project '{}' src directory is missing",
                                   proj.name));
        }
    }

    // linking stage
    for (auto proj : workSpace.projects)
    {
        if (proj.type != Project::Type::ZLIB)
            CodeGen::Link(proj);
    }
}

void ArgTokenizer::Tokenize(std::vector<std::string> args)
{
    struct Pattern
    {
        std::regex reg;
        TokenType type;
    };

    std::vector<Pattern> patterns;
    patterns.push_back({std::regex(R"(-new)"), TokenType::NEW});
    patterns.push_back({std::regex(R"(release)"), TokenType::RELEASE});
    patterns.push_back({std::regex(R"(debug)"), TokenType::DEBUG});
    patterns.push_back({std::regex(R"(-build)"), TokenType::BUILD});
    patterns.push_back({std::regex(R"(--eoa)"), TokenType::EOA});
    patterns.push_back(
        {std::regex(R"([a-zA-Z][a-zA-Z0-9_]*)"), TokenType::IDENTIFIER});
    for (std::string arg : args)
    {
        bool matched = false;
        std::smatch match;
        for (const Pattern &pattern : patterns)
        {
            if (std::regex_search(arg, match, pattern.reg,
                                  std::regex_constants::match_default))
            {
                matched = true;
                Token *token = gZtoonArena.Allocate<Token>();
                token->type = pattern.type;
                token->arg = arg;
                tokens.push_back(token);
                break;
            }
        }
        if (!matched)
        {
            PrintError(std::format("Invalid argument '{}'", arg));
        }
    }
}

ArgParser::ArgParser(std::vector<std::string> args)
{
    argTokenizer = gZtoonArena.Allocate<ArgTokenizer>();
    argTokenizer->Tokenize(args);
}
void ArgParser::Advance() { currentIndex++; }

bool ArgParser::Consume(ArgTokenizer::TokenType type)

{
    if (currentIndex < argTokenizer->tokens.size())
    {
        if (Peek()->type == type)
        {
            Advance();
            return true;
        }
    }
    return false;
}

ArgTokenizer::Token *ArgParser::Prev()
{
    size_t prevIndex = currentIndex - 1;
    if (prevIndex < 0)
    {
        prevIndex = 0;
    }
    return argTokenizer->tokens[prevIndex];
}

ArgTokenizer::Token *ArgParser::Peek()
{
    if (currentIndex < argTokenizer->tokens.size())
    {
        return argTokenizer->tokens[currentIndex];
    }
    return nullptr;
}

void ArgParser::Parse()
{
    while (!Consume(ArgTokenizer::TokenType::EOA))
    {
        if (Consume(ArgTokenizer::TokenType::NEW) &&
            buildType == ArgTokenizer::TokenType::INVALID)
        {
            if (!Consume(ArgTokenizer::TokenType::IDENTIFIER))
            {
                PrintError(std::format("Expected identifier after '-new'"));
            }
            workSpaceName = Prev()->arg;
        }
        else if (Consume(ArgTokenizer::TokenType::BUILD) &&
                 workSpaceName.empty())
        {
            if (Consume(ArgTokenizer::TokenType::RELEASE))
            {
                buildType = ArgTokenizer::TokenType::RELEASE;
            }
            else
            {
                buildType = ArgTokenizer::TokenType::DEBUG;
            }
        }
        else
        {
            PrintError(std::format("Invalid argument '{}'", Peek()->arg));
        }
    }
}

void PrintError(std::string err)
{
    printf("[Error]: %s", err.c_str());
    assert(0);
}
