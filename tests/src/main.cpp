#include "ztest.h"
#include <unordered_map>
std::unordered_map<std::string, std::function<void()>> test_funcs;
#include "lexer/lexer_test.cpp"
#include "memory_arena/memory_arena_test.cpp"
#include "parser/parser_tests.cpp"
int main()
{
    for (auto &testFunc : test_funcs)
    {
        std::cout << std::format("Running Test: {}\n", testFunc.first);
        testFunc.second();
        std::cout << std::format("[PASS]\n");
    }
    std::cout << std::format("No. Tests: {}\n", test_funcs.size());
}
