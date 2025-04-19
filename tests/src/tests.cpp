#include <cstdlib>
#include <filesystem>
#include <gtest/gtest.h>

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
