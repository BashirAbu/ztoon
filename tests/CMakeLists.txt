cmake_minimum_required(VERSION 3.20)
set (CMAKE_CXX_STANDARD 20)

set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

project(ztests)

#Fetching Google Tests
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG main
  )
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()



file (GLOB_RECURSE ZTEST_SOURCES src/*.cpp)


add_executable(${PROJECT_NAME} src/tests.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE GTest::gtest_main)
target_include_directories(${PROJECT_NAME} PRIVATE src/)

include(GoogleTest)
gtest_discover_tests(${PROJECT_NAME})
