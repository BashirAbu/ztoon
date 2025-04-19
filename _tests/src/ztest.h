#pragma once
#include <cassert>
#include <format>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
extern std::unordered_map<std::string, std::function<void()>> test_funcs;

#define TEST(name)                                                             \
    void Test_##name();                                                        \
    struct TestReg_##name                                                      \
    {                                                                          \
        TestReg_##name()                                                       \
        {                                                                      \
            assert(!test_funcs.contains(#name));                               \
            test_funcs["Test_" #name] = Test_##name;                           \
        }                                                                      \
    };                                                                         \
    TestReg_##name testReg_##name;                                             \
    void Test_##name()

#define ASSERT_EQ(value, result, fail_msg)                                     \
    if ((value) != (result))                                                   \
    {                                                                          \
        std::cout << std::format("Test \"{}\" at {}:{} failed.\n"              \
                                 "Message: {}.\n",                             \
                                 __FUNCTION__, __FILE__, __LINE__, fail_msg);  \
        exit(-1);                                                              \
    }
#define ASSERT_NE(value, result, fail_msg)                                     \
    if ((value) == (result))                                                   \
    {                                                                          \
        std::cout << std::format("Test \"{}\" at {}:{} failed.\n"              \
                                 "Message: {}.\n",                             \
                                 __FUNCTION__, __FILE__, __LINE__, fail_msg);  \
        exit(-1);                                                              \
    }

#define CALL_TEST(test_name)                                                   \
    assert(!test_funcs.contains(#test_name));                                  \
    test_funcs[#test_name]();
