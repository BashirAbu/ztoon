#include "code_gen/code_gen.h"
#include "compiler.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include <cstdio>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <stack>
#include <vector>
#include <yaml-cpp/yaml.h>
std::string basicZtoonExeMainFile =
    R"(package mainPkg;
fn main() -> i32
{
    ret 0;
}
)";

std::string basicZtoonLibraryFile =
    R"(package libPkg
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
    printIRcode = argParser->printIR;
    verbose = argParser->verbose;
    quiet = argParser->quiet;
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
            if (proj->second["type"].IsDefined())
            {
                info.type = Project::StrToPrjectType(
                    proj->second["type"].as<std::string>());
            }
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
                Project project =
                    ParseProject(proj.name, proj.relativePath, projectRoot);

                workSpace.projects.push_back(project);
            }
            else
            {
                // create project
                std::filesystem::create_directories(proj.relativePath);
                std::filesystem::create_directory(proj.relativePath / "src");
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
                case Project::Type::ZLIB:
                {
                    type = "zlib";
                    std::ofstream defFile(proj.relativePath / "src" /
                                          "lib.ztoon");
                    defFile << basicZtoonLibraryFile;
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

Project
Compiler::ParseProject(std::string projectName,
                       std::filesystem::path projectRelativePathToWorkSpace,
                       YAML::Node &projectRoot)
{
    Project project;
    project.debugBuild = argParser->buildType == ArgTokenizer::TokenType::DEBUG;
    project.relativePathToWorkSpace = projectRelativePathToWorkSpace;
    if (!projectRoot[projectName].IsDefined())
    {
        PrintError(std::format("'{}' key is not found", projectName));
    }
    project.name = projectRoot.begin()->first.as<std::string>();
    if (!projectRoot[projectName]["type"].IsDefined())
    {
        PrintError(std::format("{}'s 'type' key is not found", projectName));
    }
    project.type = Project::StrToPrjectType(
        projectRoot[projectName]["type"].as<std::string>());

    if (projectRoot[projectName]["common_flags"])
    {
    }

    if (projectRoot[projectName]["debug_flags"])
    {
        if (projectRoot[projectName]["debug_flags"]["opt_level"].IsDefined())
        {
            project.debugFlags.optLevel = project.StrToOptLevel(
                projectRoot[projectName]["debug_flags"]["opt_level"]
                    .as<std::string>());
        }
    }
    if (projectRoot[projectName]["release_flags"])
    {
        if (projectRoot[projectName]["release_flags"]["opt_level"].IsDefined())
        {
            project.releaseFlags.optLevel = project.StrToOptLevel(
                projectRoot[projectName]["release_flags"]["opt_level"]
                    .as<std::string>());
        }
    }
    if (projectRoot[projectName]["linker_flags"])
    {

        if (projectRoot[projectName]["linker_flags"]["exe_type"].IsDefined())
        {
            project.linkerFlags.SetType(
                projectRoot[projectName]["linker_flags"]["exe_type"]
                    .as<std::string>());
        }

        if (projectRoot[projectName]["linker_flags"]["no_crt"].IsDefined())
        {
            project.linkerFlags.noCRT =
                projectRoot[projectName]["linker_flags"]["no_crt"].as<bool>();
        }
        if (projectRoot[projectName]["linker_flags"]["crt_link_type"]
                .IsDefined())
        {
            project.linkerFlags.SetCRTLinkType(
                projectRoot[projectName]["linker_flags"]["crt_link_type"]
                    .as<std::string>());
        }
        if (projectRoot[projectName]["linker_flags"]["entry"].IsDefined())
        {
            project.linkerFlags.entry =
                projectRoot[projectName]["linker_flags"]["entry"]
                    .as<std::string>();
        }

        if (projectRoot[projectName]["linker_flags"]["native_libs"].IsDefined())
        {
            if (!projectRoot[projectName]["linker_flags"]["native_libs"]
                     .IsSequence())
            {
                PrintError(
                    std::format("'native_libs' key must be a sequence in "
                                "project '{}' ztoon.lib",
                                projectName));
            }
            for (auto nlMap :
                 projectRoot[projectName]["linker_flags"]["native_libs"])
            {
                Project::LinkerFlags::NativeLib nativeLibrary;
                auto nativeLib = nlMap.begin()->second;

                nativeLibrary.name = nlMap.begin()->first.as<std::string>();
                if (!nativeLib["type"].IsDefined())
                {
                    PrintError(
                        std::format("Missing 'type' key in native lib '{}' in "
                                    "'{}' "
                                    "ztoon.yaml",
                                    nativeLibrary.name, projectName));
                }
                nativeLibrary.type = project.StrToPrjectType(
                    nativeLib["type"].as<std::string>());

                if (!nativeLib["relative_path"].IsDefined())
                {
                    PrintError(
                        std::format("Missing 'relative_path' key in native lib "
                                    "'{}' in '{}' "
                                    "ztoon.yaml",
                                    nativeLibrary.name, projectName));
                }
                nativeLibrary.relative_path =
                    project.relativePathToWorkSpace /
                    nativeLib["relative_path"].as<std::string>();
                project.linkerFlags.nativeLibs.push_back(nativeLibrary);
            }
        }
    }

    if (projectRoot[projectName]["deps"])
    {
        if (!projectRoot[projectName]["deps"].IsSequence())
        {
            PrintError(std::format("{}'s 'deps' "
                                   "key must be a sequence type",
                                   projectName));
        }

        for (auto dep : projectRoot[projectName]["deps"])
        {
            auto depMap = dep.begin();
            if (!depMap->first.IsDefined())
            {
                PrintError(std::format("Missing key in '{}'s deps sequence",
                                       projectName));
            }
            Project::Dependency dependency;
            dependency.name = depMap->first.as<std::string>();
            if (!depMap->second["relative_path"].IsDefined())
            {
                PrintError(
                    std::format("'{}' key in project {}'s deps is missing",
                                dependency.name, projectName));
            }
            dependency.relativePath =
                project.relativePathToWorkSpace /
                depMap->second["relative_path"].as<std::string>();
            project.deps.push_back(dependency);
        }
    }
    if (projectRoot[projectName]["arch"].IsDefined())
    {
        project.targetArch = projectRoot[projectName]["arch"].as<std::string>();
    }
    // else
    // {
    //     // TODO remove
    //     project.targetArch = "x86_64-pc-windows-msvc";
    // }

    std::function<void(Project & project)> parseDeps = nullptr;
    parseDeps = [&](Project &_project)
    {
        for (auto &depProj : _project.deps)
        {
            std::fstream depZtoonYamlFile(depProj.relativePath / "ztoon.yaml");
            if (!depZtoonYamlFile.is_open())
            {
                PrintError(std::format(
                    "Project '{}' 'ztoon.yaml' file is missing", depProj.name));
            }

            YAML::Node depProjRoot;
            try
            {
                depProjRoot = YAML::Load(depZtoonYamlFile);
            }
            catch (const YAML::Exception &e)
            {
                std::cerr
                    << std::format(
                           "Error parsing project {}'s 'ztoon.yaml' file: ",
                           depProj.name)
                    << e.what() << "\n";
            }
            Project parsedDepProject =
                ParseProject(depProj.name, depProj.relativePath, depProjRoot);
            parsedDepProject.relativePathToWorkSpace = depProj.relativePath;
            parseDeps(parsedDepProject);
            for (auto nl : parsedDepProject.linkerFlags.nativeLibs)
            {
                Project::LinkerFlags::NativeLib lib = nl;
                lib.relative_path = lib.relative_path;

                _project.linkerFlags.nativeLibs.push_back(lib);
            }

            depProj.project = gZtoonArena.Allocate<Project>();
            *depProj.project = parsedDepProject;
        }
    };
    parseDeps(project);
    return project;
}

void Compiler::BuildProject(Project &project)
{
    if (!quiet)
    {
        PrintMSG(std::format("Building project '{}' ...", project.name));

        if (verbose)
        {
            // Arch
            // PrintMSG(std::format("Target architecture: {}", ));
            // build type
            PrintMSG(std::format("Type: {}",
                                 Project::PrjectTypeToStr(project.type)));
            if (project.debugBuild)
            {
                PrintMSG("Build type: Debug");
                // Compiler Flags
                PrintMSG("Compiler Flags:");
                PrintMSG(std::format(
                    "Optimizatoin Level: {}",
                    project.OptLevelToStr(project.debugFlags.optLevel)));
            }
            else
            {
                PrintMSG("Build type: Release");
                PrintMSG("Compiler Flags:");
                PrintMSG(std::format(
                    "Optimizatoin Level: {}",
                    project.OptLevelToStr(project.releaseFlags.optLevel)));
            }

            // linker flags
            PrintMSG("Linker Flags:");

            if (project.type == Project::Type::EXE)
            {
                PrintMSG(std::format("Executable Type: '{}'",
                                     project.linkerFlags.type ==
                                             Project::LinkerFlags::Type::CONSOLE
                                         ? "console"
                                         : "window"));
            }
            PrintMSG(std::format("Linking against CRT: '{}'",
                                 project.linkerFlags.noCRT ? "Off" : "On"));
            if (!project.linkerFlags.noCRT)
            {
                PrintMSG(std::format(
                    "CRT link type: {}",
                    project.linkerFlags.crtLinkType ==
                            Project::LinkerFlags::CRT_LinkType::STATIC
                        ? "static"
                        : "dynamic"));
            }
            if (!project.linkerFlags.nativeLibs.empty())
            {
                PrintMSG("Liking against native libs:");
                for (auto nativeLib : project.linkerFlags.nativeLibs)
                {
                    PrintMSG(std::format("Name: {}", nativeLib.name));
                    PrintMSG(std::format(
                        "Type: {}", Project::PrjectTypeToStr(nativeLib.type)));
                }
                PrintMSG("");
            }

            if (!project.deps.empty())
            {
                PrintMSG("Ztoon libs depedencies:");
                for (auto dep : project.deps)
                {
                    PrintMSG(dep.name);
                }
            }
        }
    }

    std::function<std::vector<Package *>(Project & project)>
        lexerAndParseProject = nullptr;

    lexerAndParseProject = [&](Project &project) -> std::vector<Package *>
    {
        auto srcPath = project.relativePathToWorkSpace / "src";
        if (std::filesystem::exists(srcPath) &&
            std::filesystem::is_directory(srcPath))
        {
            std::vector<Package *> projectPackages;
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
                    Lexer lexer;
                    lexer.Tokenize(content, filename);
                    lexer.EndProgram();
                    Parser parser(lexer.GetTokens());
                    auto packages = parser.Parse();
                    projectPackages.insert(projectPackages.end(),
                                           packages.begin(), packages.end());
                }
            }

            std::unordered_map<std::string, bool> done;
            auto pkgItr = projectPackages.begin();
            while (pkgItr != projectPackages.end())
            {
                std::string pkgName = (*pkgItr)->GetIdentifier()->GetLexeme();
                if (done.contains(pkgName))
                {
                    pkgItr = projectPackages.erase(pkgItr);
                    continue;
                }

                for (auto innerPkg : projectPackages)
                {
                    if (innerPkg->GetIdentifier()->GetLexeme() == pkgName)
                    {
                        if (innerPkg != (*pkgItr))
                        {
                            for (auto stmt : innerPkg->GetStatements())
                            {
                                (*pkgItr)->GetStatements().push_back(stmt);
                            }
                        }
                    }
                }

                // we done with this package
                done[pkgName] = true;
                ++pkgItr;
            }

            return projectPackages;
        }
        else
        {
            PrintError(std::format("Project '{}' src directory is missing",
                                   project.name));
            return {};
        }
    };

    std::function<std::vector<Library *>(Project & project)> goOverDeps;
    goOverDeps = [&](Project &project) -> std::vector<Library *>
    {
        std::vector<Library *> libs;
        for (auto dep : project.deps)
        {
            Library *lib = gZtoonArena.Allocate<Library>();
            lib->name = dep.name;
            lib->packages = lexerAndParseProject(*dep.project);
            lib->libs = goOverDeps(*dep.project);
            libs.push_back(lib);
        }
        return libs;
    };

    auto packages = lexerAndParseProject(project);
    auto libs = goOverDeps(project);

    for (auto lib : libs)
    {
        for (auto otherLib : libs)
        {
            if (lib != otherLib)
            {
                if (lib->name == otherLib->name)
                {
                    PrintError(std::format(
                        "Libraries '{}' and '{}' have the same names",
                        lib->name, otherLib->name));
                }
            }
        }
    }
    for (auto lib : libs)
    {
        for (auto pkg : packages)
        {
            if (lib->name == pkg->GetIdentifier()->GetLexeme())
            {
                PrintError(std::format(
                    "Package '{}' has same name as library '{}'. Try changing "
                    "the name of the libarry in project '{}' ztoon.yaml",
                    pkg->GetIdentifier()->GetLexeme(), lib->name,
                    project.name));
            }
        }
    }
    std::vector<std::string> ulibNames;
    std::vector<Library *> ulibs;
    for (auto lib : libs)
    {
        std::stack<Library *> libs;
        libs.push(lib);
        while (!libs.empty())
        {
            Library *l = libs.top();
            libs.pop();

            for (auto il : l->libs)
            {
                libs.push(il);
            }

            if (std::find(ulibNames.begin(), ulibNames.end(), l->name) ==
                ulibNames.end())
            {
                ulibNames.push_back(l->name);
                ulibs.push_back(l);
            }
        }
    }

    SemanticAnalyzer semanticAnalyzer(packages, ulibs);
    CodeGen codeGen(semanticAnalyzer);
    PrintMSG("Compiling...");
    codeGen.Compile(project, printIRcode);
}

void Compiler::BuildWorkSpace()
{

    if (!argParser->buildThisProject.empty())
    {

        Project buildProj;
        bool found = false;
        for (auto proj : workSpace.projects)
        {
            if (proj.name == argParser->buildThisProject)
            {
                buildProj = proj;
                found = true;
                break;
            }
        }

        if (!found)
        {
            PrintError(std::format("Project '{}' is not found",
                                   argParser->buildThisProject));
        }

        BuildProject(buildProj);

        if (buildProj.type != Project::Type::ZLIB)
        {
            PrintMSG(std::format("Linking {}...", buildProj.name));
            CodeGen::Link(buildProj);
            PrintMSG("Finished.");
        }

        return;
    }

    // go over each project
    for (auto proj : workSpace.projects)
    {
        if (proj.type != Project::Type::ZLIB)
            BuildProject(proj);
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
    patterns.push_back({std::regex(R"(-project)"), TokenType::PROJECT});
    patterns.push_back({std::regex(R"(-ir)"), TokenType::IR});
    patterns.push_back({std::regex(R"(-v)"), TokenType::V});
    patterns.push_back({std::regex(R"(-q)"), TokenType::Q});
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
        else if (Consume(ArgTokenizer::TokenType::PROJECT))
        {
            if (!Consume(ArgTokenizer::TokenType::IDENTIFIER))
            {
                PrintError(std::format("Expected project name after '-new'"));
            }
            buildThisProject = Prev()->arg;
        }
        else if (Consume(ArgTokenizer::TokenType::V))
        {
            verbose = true;
        }
        else if (Consume(ArgTokenizer::TokenType::IR))
        {
            printIR = true;
        }
        else if (Consume(ArgTokenizer::TokenType::Q))
        {
            quiet = true;
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
    exit(-1);
    // assert(0);
}
void PrintMSG(std::string msg) { printf("%s\n", msg.c_str()); }
