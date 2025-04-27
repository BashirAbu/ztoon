@echo off

mkdir build

pushd build

cmake -G "Ninja" .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DLLVM_DIR=D:\llvm_install_path\lib\cmake\llvm\ -DClang_DIR=D:\llvm_install_path\lib\cmake\clang\ -DLLD_DIR=D:\llvm_install_path\lib\cmake\lld\ -Wno-dev
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -Wno-dev ..
ninja -j0
popd
