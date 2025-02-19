#include "error_report.h"
#include "lexer/lexer.h"
#include <format>
#include <iostream>
#include <stdlib.h>
void ReportError(std::string errorMsg, Token const *token)
{
    if (token)
    {
        std::cerr << std::format("Error at {}:{}\n{}\n", token->GetFilename(),
                                 token->GetLineNumber(), token->GetLineStr());
        std::string errorPointer = "";
        for (size_t i = 1; i < token->GetColNumber(); i++)
        {
            errorPointer += " ";
        }

        for (size_t i = 0; i < token->GetLexeme().length(); i++)
        {
            errorPointer += "^";
        }

        std::cerr << std::format("{}\n{}.", errorPointer, errorMsg);
    }
    else
    {
        std::cerr << std::format("Error: {}", errorMsg);
    }
    exit(-1);
}
