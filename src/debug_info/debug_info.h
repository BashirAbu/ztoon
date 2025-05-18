#pragma once
#include "code_gen/code_gen.h"
#include "parser/parser.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include <stack>
#include <unordered_map>
class DebugInfo
{
  public:
    DebugInfo(CodeGen *codeGen);
    ~DebugInfo();

    void Finalize();

    llvm::DICompileUnit *GetCU(Package *pkg);
    llvm::DIFile *GetDIFile(std::string filepath);

    void GenFnStatementDI(FnStatement *fnStmt, FnExpression *fnExpr,
                          IRFunction *irFunc);
    void GenVarDeclStatementDI(VarDeclStatement *varStmt,
                               IRVariable *irVariable, bool isGlobal);
    void SetDebugLoc(Token const* token);
    llvm::DIScope *currentScope = nullptr;
  private:
    llvm::DIType *ZtoonTypeToDIType(DataType *type);

    CodeGen *codeGen = nullptr;
    std::unique_ptr<llvm::DIBuilder> diBuilder;
    std::unordered_map<Package *, llvm::DICompileUnit *> pkgToCompUnit;
    std::unordered_map<std::string, llvm::DIFile *> filepathToDIFile;
    std::unordered_map<std::string, llvm::DIType *> ztoonTypeToDITypeMap;
    friend class CodeGen;
};
