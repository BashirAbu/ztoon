#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <unordered_map>

struct IRType
{
    bool isSigned = false;
    llvm::Type *type = nullptr;
};
struct IRVariable
{
    Variable const *variabel = nullptr;
    llvm::AllocaInst *aInsta = nullptr;
    IRType irType;
};

struct IRValue
{
    llvm::Value *value = nullptr;
    IRType type;
};

class CodeGen
{
  public:
    CodeGen(const SemanticAnalyzer &semanticAnalyzer);
    ~CodeGen();
    void GenIR();
    void AddIRVariable(IRVariable *irVariable);
    IRVariable *GetIRVariable(std::string name);

    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> irBuilder;
    std::unique_ptr<llvm::DataLayout> moduleDataLayout;

  private:
    IRValue GenExpressionIR(Expression *expression);
    IRValue CastIRValue(IRValue value, IRType castType);
    IRType TokenTypeToLLVMType(TokenType type);

    IRValue CastIntToInt(IRValue value, IRType castType);
    IRValue CastFloatToFloat(IRValue value, IRType castType);
    IRValue CastFloatToInt(IRValue value, IRType castType);
    IRValue CastIntToFloat(IRValue value, IRType castType);

    std::unordered_map<std::string, IRVariable *> irVariablesMap;
    const SemanticAnalyzer &semanticAnalyzer;
};
