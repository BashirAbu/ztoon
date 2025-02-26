echo Ztoon LOC:
find src/ -type f -name '*' | xargs wc -l
echo Tests LOC:
find tests/src/ -type f -name '*' | xargs wc -l
