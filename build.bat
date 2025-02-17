@echo off

mkdir build

pushd build

cmake -G "Ninja" .. -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DLLVM_DIR=D:\llvm_installd\lib\cmake\llvm\
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
ninja -j0
popd
