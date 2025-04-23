@echo off
pushd tests\workspaces
python ..\..\gtest-parallel\gtest_parallel.py  ..\..\build\tests\ztests.exe
popd
