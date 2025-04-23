#include <cstdlib>
#include <filesystem>
#include <gtest/gtest.h>

TEST(ZtoonBasics, Integers)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "integers";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, BasicArithmetics)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();

    std::filesystem::current_path("basics/");

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project basic_arithmetics");

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\basic_arithmetics";

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, BooleanOps)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "boolean_ops";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, ShiftOps)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "shift_ops";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, BitwiseOps)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "bitwise_ops";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, ForLoop)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();

    std::filesystem::current_path("basics/");

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project for_loop");

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\for_loop";

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, ForLoopBreak)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "for_loop_break";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, ForLoopContinue)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "for_loop_continue";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, WhileLoop)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "while_loop";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, WhileLoopContinue)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "while_loop_continue";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, WhileLoopBreak)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "while_loop_break";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, NestedLoops)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "nested_loops";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, NestedLoopsBreakContinue)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "nested_loops_break_continue";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}

TEST(ZtoonBasics, if_statement)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "if_statement";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, IfStatement)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "if_statement";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, TernaryExpression)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "ternary_expression";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}

TEST(ZtoonBasics, UnaryMinus)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "unary_minus";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, UnarySizeof)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "unary_sizeof";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, CompoundAssignment)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "compound_assignment";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}

TEST(ZtoonBasics, Casting)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "casting";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Functions)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "functions";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Recursion)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "recursion";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Pointers)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "pointers";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, UninitializedVariables)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "uninitialized_variables";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, GlobalVariables)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "global_variables";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Readonly)
{
    // call compiler, call binary, check ret value

    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "readonly";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, -1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Arrays)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "arrays";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, ArrayMultidimensions)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "arrays_multi_dimensions";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Strings)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "strings";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, FunctionPointers)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "function_pointers";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Structs)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "structs";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
TEST(ZtoonBasics, Unions)
{
    // call compiler, call binary, check ret value
    auto oldPath = std::filesystem::current_path();
    std::string workspace = "basics";
    std::string project = "unions";
    std::filesystem::current_path(workspace);

    std::string compilerCommand =
        ("..\\..\\..\\build\\ztoon -build -project " + project);

    int compilerRet = std::system(compilerCommand.c_str());

    EXPECT_EQ(compilerRet, 0);

    std::string binaryCommand = "bin\\" + project;

    int binaryRet = std::system(binaryCommand.c_str());

    EXPECT_EQ(binaryRet, 1);

    std::filesystem::current_path(oldPath);
}
// TEST(ZtoonBasics, Enums)
// {
//     // call compiler, call binary, check ret value
//     auto oldPath = std::filesystem::current_path();
//     std::string workspace = "enums";
//     std::string project = "function_pointers";
//     std::filesystem::current_path(workspace);

//     std::string compilerCommand =
//         ("..\\..\\..\\build\\ztoon -build -project " + project);

//     int compilerRet = std::system(compilerCommand.c_str());

//     EXPECT_EQ(compilerRet, 0);

//     std::string binaryCommand = "bin\\" + project;

//     int binaryRet = std::system(binaryCommand.c_str());

//     EXPECT_EQ(binaryRet, 1);

//     std::filesystem::current_path(oldPath);
// }
