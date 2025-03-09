#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
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

struct IfStatementData
{
    llvm::BasicBlock *falseBlock = nullptr, *mergeBlock = nullptr;
    llvm::Function *currentFunction = nullptr;
};

struct IRFunction
{
    llvm::FunctionType *fnType = nullptr;
    llvm::Function *fn = nullptr;
    llvm::BasicBlock *fnBB;
    Function *ztoonFn = nullptr;
};
class CodeGen
{
  public:
    CodeGen(SemanticAnalyzer &semanticAnalyzer, std::string targetArch);
    ~CodeGen();
    void GenIR();
    void AddIRVariable(IRVariable *irVariable);
    IRVariable *GetIRVariable(std::string name);

    void AddIRFunction(IRFunction *irFunc);
    IRFunction *GetIRFunction(std::string name, CodeErrString codeErrString);

    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> irBuilder;
    std::unique_ptr<llvm::DataLayout> moduleDataLayout;

  private:
    IRValue GetLValue(Expression *expr);
    uint32_t GetPtrBitWidth()
    {
        return moduleDataLayout->getPointerSizeInBits();
    }
    void GenStatementIR(Statement *statement);
    void GenIfStatementIR(Statement *statement, IfStatementData *ifData);
    IRValue GenExpressionIR(Expression *expression);
    IRValue CastIRValue(IRValue value, IRType castType);
    IRType ZtoonTypeToLLVMType(DataType *type);
    bool IsNaN(llvm::Value *value);
    llvm::CmpInst::Predicate CMPZtoonTypeToCMPLLVM(TokenType type,
                                                   IRValue value, bool isNaN);
    IRValue CastIntToInt(IRValue value, IRType castType);
    IRValue CastFloatToFloat(IRValue value, IRType castType);
    IRValue CastFloatToInt(IRValue value, IRType castType);
    IRValue CastIntToFloat(IRValue value, IRType castType);
    IRValue CastPtrToPtr(IRValue value, IRType castType);
    IRValue CastIntToPtr(IRValue value, IRType castType);
    IRValue CastPtrToInt(IRValue value, IRType castType);
    std::unordered_map<Scope const *,
                       std::unordered_map<std::string, IRVariable *>>
        scopeToIRVariablesMap;
    std::unordered_map<std::string, IRVariable *> irVariablesMap;
    std::unordered_map<std::string, IRFunction *> irFunctionsMaps;

    SemanticAnalyzer &semanticAnalyzer;
};
