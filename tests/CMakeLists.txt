
cmake_minimum_required(VERSION 3.20)
set (CMAKE_CXX_STANDARD 20)

set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

project(ztests)

file (GLOB_RECURSE ZTEST_SOURCES src/main.cpp)


add_executable(${PROJECT_NAME} ${ZTEST_SOURCES})
target_link_libraries(${PROJECT_NAME} PRIVATE ztoon-lib)
target_include_directories(${PROJECT_NAME} PRIVATE src/)


