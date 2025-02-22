cmake_minimum_required(VERSION 3.20)
set (CMAKE_CXX_STANDARD 20)
set(CMAKE_SUPPRESS_DEVELOPER_WARNINGS ON)
set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.
message(STATUS "DIR: " ${LLVM_INCLUDE_DIRS})
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

project(ztoon)

file (GLOB_RECURSE ZTOON_SOURCES src/*.cpp src/*.c)
list(REMOVE_ITEM ${ZTOON_SOURCES} src/main.cpp)

add_library(${PROJECT_NAME}-lib STATIC ${ZTOON_SOURCES})
target_include_directories(${PROJECT_NAME}-lib PUBLIC src/)

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(${PROJECT_NAME}-lib PUBLIC ${llvm_libs})


add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}-lib)


add_subdirectory(tests)
