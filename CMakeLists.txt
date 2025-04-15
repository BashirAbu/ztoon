cmake_minimum_required(VERSION 3.20)
set (CMAKE_CXX_STANDARD 20)
set(CMAKE_SUPPRESS_DEVELOPER_WARNINGS ON)
set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)
find_package(LLVM REQUIRED CONFIG)
find_package(LLD REQUIRED)

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
include(FetchContent)

FetchContent_Declare(
  yaml-cpp
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG master )
FetchContent_MakeAvailable(yaml-cpp)

project(ztoon)
file (GLOB_RECURSE ZTOON_SOURCES src/*.cpp src/*.c third_party/*.cpp third_party/*.c)
list(REMOVE_ITEM ${ZTOON_SOURCES} src/main.cpp)

add_library(${PROJECT_NAME}-lib STATIC ${ZTOON_SOURCES})
target_include_directories(${PROJECT_NAME}-lib PUBLIC src/ third_party/)

llvm_map_components_to_libnames(llvm_libs support core irreader native orcjit
   option
   AArch64 AMDGPU ARM AVR BPF Hexagon Lanai LoongArch Mips MSP430 NVPTX PowerPC
   RISCV Sparc SystemZ VE WebAssembly X86 XCore)

set (lld_libs lldCOFF lldELF lldCommon lldMachO lldMinGW lldWasm)
target_link_libraries(${PROJECT_NAME}-lib PUBLIC ${llvm_libs} ${lld_libs} yaml-cpp::yaml-cpp)


add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}-lib)


#add_subdirectory(tests)
