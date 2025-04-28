#include "compiler/compiler.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Target/TargetMachine.h"
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

struct IRReadonlySymbol : public IRSymbol
{
    std::string name;
    llvm::Value *value;
    IRType irType;
    virtual std::string GetName() override { return name; }
    virtual llvm::Value *GetValue() override { return value; }
    virtual llvm::Type *GetType() override { return irType.type; }
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
    std::string fullName;
    std::string name;
    virtual std::string GetName() override { return name; }
    virtual std::string GetFullName() { return fullName; }
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
    CodeGen(SemanticAnalyzer &semanticAnalyzer);
    ~CodeGen();
    void GenIR(std::vector<Package *> &packages, bool genSymbol,
               bool genSymbolBody);
    void Compile(Project &project, bool printIR = false);
    static void Link(Project &project);
    void AddIRSymbol(IRSymbol *irSymbol);
    IRSymbol *GetIRSymbol(std::string name);
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> irBuilder;
    std::unique_ptr<llvm::DataLayout> moduleDataLayout;

  private:
    void AssignValueToVarArray(IRValue ptr, Expression *expr,
                               ArrayDataType *arrType,
                               std::vector<llvm::Value *> &index);
    void AssignValueToVarStruct(IRValue ptr, Expression *expr,
                                StructDataType *structType);
    uint32_t GetPtrBitWidth()
    {
        return moduleDataLayout->getPointerSizeInBits();
    }
    llvm::Constant *
    InitListToArrayConstant(ArrayDataType *arrType,
                            InitializerListExpression *listExpr);
    llvm::Constant *
    InitListToStructConstant(StructDataType *structType,
                             InitializerListExpression *listExpr);
    void GenPackageGlobalTypesIR(Package *pkg);
    void GenPackageGlobalFuncsAndVarsIR(Package *pkg);
    void GenGlobalVariableIR(VarDeclStatement *varDeclStatement);
    void GenPackageGlobalVarAndFuncBodiesIR(Package *pkg);
    void GenStatementIR(Statement *statement);

    void GenBlockStatementIR(BlockStatement *blockStmt);
    void GenFnStatementIR(FnStatement *fnStmt);
    void GenVarDeclStatementIR(VarDeclStatement *varDeclStmt, bool genSymbol,
                               bool genBody);
    void GenVarAssignmentStatementIR(VarAssignmentStatement *varAssignmentStmt);
    void GenVarCompundAssignmentStatementIR(
        VarCompoundAssignmentStatement *varComStmt);
    void GenExpressionStatementIR(ExpressionStatement *exprStmt);
    void GenIfStatementIR(IfStatement *ifStmt);
    void GenElseIfStatementIR(ElseIfStatement *elifStmt);
    void GenElseStatementIR(ElseStatement *elseStmt);
    void GenSwitchStatementIR(SwitchStatement *switchStmt);
    void GenWhileLoopStatementIR(WhileLoopStatement *whileLoopStmt);
    void GenForLoopStatementIR(ForLoopStatement *forLoopStmt);
    void GenBreakStatementIR(BreakStatement *breakStmt);
    void GenContinueStatementIR(ContinueStatement *continueStmt);
    void GenStructStatementIR(StructStatement *structStmt, bool genSymbol,
                              bool genBody);
    void GenUnionStatementIR(UnionStatement *unionStmt, bool genSymbol,
                             bool genBody);
    void GenEnumStatementIR(EnumStatement *enumStmt, bool genSymbol,
                            bool genBody);
    void GenRetStatementIR(RetStatement *retStmt);
    void GenImportStatementIR(ImportStatement *importStmt);

    IRValue GenFnExpressionIR(FnExpression *fnExpression, bool isWrite);
    IRValue GenTernaryExpressionIR(TernaryExpression *ternaryExpr,
                                   bool isWrite);
    IRValue GenBinaryExpressionIR(BinaryExpression *binaryExpr, bool isWrite);
    IRValue GenCastExpressionIR(CastExpression *castExpr, bool isWrite);
    IRValue GenGroupingExpressionIR(GroupingExpression *groupingEpxr,
                                    bool isWrite);
    IRValue GenSubScriptExpressionIR(SubscriptExpression *subExpr,
                                     bool isWrite);
    IRValue GenUnaryExpressionIR(UnaryExpression *unaryExpr, bool isWrite);
    IRValue GenFnCallExpressionIR(FnCallExpression *fnCallExpr, bool isWrite);
    IRValue
    GenInitializerListExpressionIR(InitializerListExpression *initListEpxr,
                                   bool isWrite);
    IRValue GenMemberAccessExpressionIR(MemberAccessExpression *maExpr,
                                        bool isWrite);
    IRValue GenPrimaryExpressionIR(PrimaryExpression *primaryExpr,
                                   bool isWrite);

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
    std::unordered_map<std::string, IRSymbol *> irSymbolsMap;
    IRLoop *currentLoop = nullptr;
    llvm::Value *startOfStackFrame = nullptr;
    SemanticAnalyzer &semanticAnalyzer;
    std::unordered_map<Statement *, bool> globalStatementIRDoneMap;
    std::unordered_map<VarDeclStatement *, IRValue> globalConstsMap;
    std::unordered_map<Symbol *, IRSymbol *> symbolToIRSymobMap;
};
