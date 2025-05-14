mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
-DLLVM_DIR=/media/bashir/C424B9A524B99ABE/llvm_linux/lib/cmake/llvm/  \
-DLLD_DIR=/media/bashir/C424B9A524B99ABE/llvm_linux/lib/cmake/lld/ -Wno-dev\
  -DZLIB_LIBRARY=/usr/lib/x86_64-linux-gnu/libz.so \
  -DZLIB_INCLUDE_DIR=/usr/include \
  -Dzstd_LIBRARY=/usr/lib/x86_64-linux-gnu/libzstd.so \
  -Dzstd_INCLUDE_DIR=/usr/include
make -j15
cd ..
