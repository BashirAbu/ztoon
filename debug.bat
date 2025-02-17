@echo off

@echo off
set break_point=%1
start cmd /c raddbg -q
start cmd /c raddbg --ipc run_to_line %break_point%
