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

struct IRSymbol
{
    virtual ~IRSymbol() {};
    virtual std::string GetName() = 0;
    virtual llvm::Value *GetValue() = 0;
    virtual llvm::Type *GetType() = 0;
};

struct IRVariable : public IRSymbol
{
    Variable *variabel = nullptr;
    llvm::Value *value = nullptr;
    IRType irType;
    virtual std::string GetName() override { return variabel->GetName(); }
    virtual llvm::Value *GetValue() override { return value; }
    virtual llvm::Type *GetType() override { return irType.type; }
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

struct IRFunction : public IRSymbol
{
    llvm::FunctionType *fnType = nullptr;
    llvm::Function *fn = nullptr;
    llvm::BasicBlock *fnBB;
    Function *ztoonFn = nullptr;
    virtual std::string GetName() override { return ztoonFn->GetName(); }
    virtual llvm::Value *GetValue() override { return fn; }
    virtual llvm::Type *GetType() override { return fnType; }
};

struct IRLoop
{
    llvm::BasicBlock *loopBB = nullptr;
    llvm::BasicBlock *extBB = nullptr;
    llvm::BasicBlock *condBB = nullptr;

    Statement *loopStmt = nullptr;
};
class CodeGen
{
  public:
    CodeGen(SemanticAnalyzer &semanticAnalyzer, std::string targetArch);
    ~CodeGen();
    void GenIR();

    void AddIRSymbol(IRSymbol *irSymbol);
    IRSymbol *GetIRSymbol(std::string name);
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> irBuilder;
    std::unique_ptr<llvm::DataLayout> moduleDataLayout;

  private:
    // IRValue GetLValue(Expression *expr);
    uint32_t GetPtrBitWidth()
    {
        return moduleDataLayout->getPointerSizeInBits();
    }
    bool CheckArraySizeAtDeclaration(PointerDataType *type,
                                     InitializerListExpression *initExpr);
    void StoreInitListInArray(IRValue ptr, InitializerListExpression *listExpr);
    llvm::Constant *
    InitListToArrayConstant(IRType arrayType,
                            InitializerListExpression *listExpr);
    void GenStatementIR(Statement *statement);
    void GenIfStatementIR(Statement *statement, IfStatementData *ifData);
    IRValue GenExpressionIR(Expression *expression, bool isWrite = false);
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
                       std::unordered_map<std::string, IRSymbol *>>
        scopeToIRSymbolsMap;
    std::unordered_map<std::string, IRSymbol *> irSymbolsMap;
    IRLoop *currentLoop = nullptr;
    SemanticAnalyzer &semanticAnalyzer;
};
