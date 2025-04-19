#include "error_report.h"
#include "lexer/lexer.h"
#include <format>
#include <iostream>
#include <stdlib.h>
void ReportError(std::string errorMsg, CodeErrString errStr)
{
    std::cerr << std::format(
        "Error at {}:{}\n{}\n", errStr.firstToken->GetFilename(),
        errStr.firstToken->GetLineNumber(), errStr.firstToken->GetLineStr());
    std::string errorPointer = "";
    for (size_t i = 1; i < errStr.firstToken->GetColNumber(); i++)
    {
        errorPointer += " ";
    }

    errorPointer += "^";

    std::cerr << std::format("{}\n{}.", errorPointer, errorMsg);

    // #ifdef _DEBUG
    //     assert(0);
    // #else
    exit(-1);
    // #endif
}
