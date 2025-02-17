cmake_minimum_required(VERSION 3.20)
set (CMAKE_CXX_STANDARD 20)

set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

project(ztoon)

file (GLOB_RECURSE ZTOON_SOURCES src/*.cpp src/*.c)
list(REMOVE_ITEM ${ZTOON_SOURCES} src/main.cpp)

add_library(${PROJECT_NAME}-lib STATIC ${ZTOON_SOURCES})
target_include_directories(${PROJECT_NAME}-lib PUBLIC src/)

add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}-lib)


add_subdirectory(tests)
