#pragma once
#include "code_gen/code_gen.h"
#include "parser/parser.h"
#include "llvm/IR/DebugInfoMetadata.h"

class DebugInfo
{
  public:
    DebugInfo(CodeGen *codeGen);
    ~DebugInfo();

    void Finialize();

    llvm::DICompileUnit *GetCU(Package *pkg);
    llvm::DIFile *GetDIFile(std::string filepath);

    void GenFnStatementDI(FnStatement *fnStmt, IRFunction *irFunc);
    void GenBlockStatementDI(BlockStatement *blockStmt);

  private:
    llvm::DIType *ZtoonTypeToDIType(DataType *type);

    llvm::DIScope *currentScope = nullptr;

    CodeGen *codeGen = nullptr;
    std::unique_ptr<llvm::DIBuilder> diBuilder;
    std::unordered_map<Package *, llvm::DICompileUnit *> pkgToCompUnit;
    std::unordered_map<std::string, llvm::DIFile *> filepathToDIFile;
};
