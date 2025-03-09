#include "code_gen.h"
#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <format>
#include <memory>
#include <type_traits>
IRType CodeGen::ZtoonTypeToLLVMType(DataType *type)
{
    IRType irType = {};
    switch (type->GetType())
    {

    case DataType::Type::I8:
    {
        irType.type = irBuilder->getIntNTy(8);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I16:
    {
        irType.type = irBuilder->getIntNTy(16);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I32:
    {
        irType.type = irBuilder->getIntNTy(32);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I64:
    {
        irType.type = irBuilder->getIntNTy(64);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::U8:
    {
        irType.type = irBuilder->getIntNTy(8);
        break;
    }
    case DataType::Type::U16:
    {
        irType.type = irBuilder->getIntNTy(16);
        break;
    }
    case DataType::Type::U32:
    {
        irType.type = irBuilder->getIntNTy(32);
        break;
    }
    case DataType::Type::U64:
    {
        irType.type = irBuilder->getIntNTy(64);
        break;
    }
    case DataType::Type::F32:
    {
        irType.type = irBuilder->getFloatTy();
        break;
    }
    case DataType::Type::F64:
    {
        irType.type = irBuilder->getDoubleTy();
        break;
    }
    case DataType::Type::BOOL:
    {
        irType.type = irBuilder->getIntNTy(1);
        break;
    }
    case DataType::Type::NOTYPE:
    {
        irType.type = irBuilder->getVoidTy();
        break;
    }
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
    {
        irType.type = irBuilder->getIntNTy(1);
        break;
    }
    case DataType::Type::POINTER:
    {
        auto ptrZtoonType = dynamic_cast<PointerDataType *>(type);
        IRType dataType = ZtoonTypeToLLVMType(ptrZtoonType->dataType);
        irType.type = dataType.type->getPointerTo();
        break;
    }
    default:
    {
        break;
    }
    }

    return irType;
}

void CodeGen::AddIRVariable(IRVariable *irVariable)
{
    assert(irVariable);
    if (!scopeToIRVariablesMap.contains(semanticAnalyzer.currentScope))
    {
        scopeToIRVariablesMap[semanticAnalyzer.currentScope] = {};
    }

    (scopeToIRVariablesMap[semanticAnalyzer.currentScope])[irVariable->variabel
                                                               ->GetName()] =
        irVariable;
}

IRVariable *CodeGen::GetIRVariable(std::string name)
{
    IRVariable *ret = nullptr;
    Scope const *scope = semanticAnalyzer.currentScope;
    while (!ret)
    {
        if ((scopeToIRVariablesMap[scope]).contains(name))
        {
            IRVariable *ret = (scopeToIRVariablesMap[scope])[name];
            return ret;
        }
        scope = scope->GetParent();
        if (!scope)
        {
            break;
        }
    }

    assert(0);
    return nullptr;
}

void CodeGen::AddIRFunction(IRFunction *irFunc)
{
    assert(irFunc);
    if (!irFunctionsMaps.contains(irFunc->ztoonFn->GetName()))
    {
        irFunctionsMaps[irFunc->ztoonFn->GetName()] = irFunc;
    }
    else
    {
        ReportError(std::format("Function '{}' is already defined",
                                irFunc->ztoonFn->GetName()),
                    irFunc->ztoonFn->GetFnStatement()->GetCodeErrString());
    }
}

IRFunction *CodeGen::GetIRFunction(std::string name,
                                   CodeErrString codeErrString)
{
    if (irFunctionsMaps.contains(name))
    {
        return irFunctionsMaps[name];
    }

    CodeErrString ces = {};

    ReportError(std::format("Function '{}' is not defined", name),
                codeErrString);

    assert(0);
    return nullptr;
}
IRValue CodeGen::CastIntToInt(IRValue value, IRType castType)
{
    IRValue castedValue = {};
    if ((value.type.type == castType.type) &&
        (value.type.isSigned == castType.isSigned))
    {
        return value;
    }

    uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
    uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();

    if (valueBitWidth > castTypeBitWidth)
    {
        // truncate value
        castedValue.value = irBuilder->CreateTrunc(value.value, castType.type);
    }
    else if (valueBitWidth < castTypeBitWidth)
    {
        castedValue.value =
            value.type.isSigned
                ? irBuilder->CreateSExt(value.value, castType.type)
                : irBuilder->CreateZExt(value.value, castType.type);
    }
    else
    {
        castedValue.value = value.value;
    }

    castedValue.type = castType;
    return castedValue;
}
IRValue CodeGen::CastFloatToFloat(IRValue value, IRType castType)
{

    IRValue castedValue = {};
    if (value.type.type == castType.type)
    {
        return value;
    }

    uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
    uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;

    if (valueBitWidth > castTypeBitWidth)
    {
        // truncate value
        castedValue.value =
            irBuilder->CreateFPTrunc(value.value, castType.type);
    }
    else
    {
        castedValue.value = irBuilder->CreateFPExt(value.value, castType.type);
    }

    castedValue.type = castType;
    return castedValue;
}
IRValue CodeGen::CastFloatToInt(IRValue value, IRType castType)
{
    IRValue castedValue = {};
    uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
    uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();
    castedValue.type = castType;
    castedValue.value =
        castType.isSigned ? irBuilder->CreateFPToSI(value.value, castType.type)
                          : irBuilder->CreateFPToUI(value.value, castType.type);
    castedValue = CastIntToInt(castedValue, castType);
    return castedValue;
}
IRValue CodeGen::CastIntToFloat(IRValue value, IRType castType)
{

    IRValue castedValue = {};
    uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
    uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;
    castedValue.type = castType;
    castedValue.value =
        value.type.isSigned
            ? irBuilder->CreateSIToFP(value.value, castType.type)
            : irBuilder->CreateUIToFP(value.value, castType.type);
    castedValue = CastFloatToFloat(castedValue, castType);
    return castedValue;
}

IRValue CodeGen::CastPtrToPtr(IRValue value, IRType castType)
{
    IRValue irValue = {};
    irValue.type = castType;
    irValue.value = irBuilder->CreateBitCast(value.value, castType.type);

    return irValue;
}
IRValue CodeGen::CastIntToPtr(IRValue value, IRType castType)
{
    IRValue irValue = {};
    // first cast valut int to int with arch bit width.
    IRType intType = {};
    intType.type = llvm::Type::getIntNTy(*ctx, GetPtrBitWidth());
    irValue = CastIntToInt(value, intType);
    irValue.value = irBuilder->CreateIntToPtr(value.value, castType.type);
    irValue.type = castType;

    return irValue;
}
IRValue CodeGen::CastPtrToInt(IRValue value, IRType castType)
{

    IRValue irValue = {};
    irValue.value = irBuilder->CreatePtrToInt(value.value, castType.type);
    irValue.type = castType;

    return irValue;
}
IRValue CodeGen::CastIRValue(IRValue value, IRType castType)
{
    // int to int
    if (value.type.type->isIntegerTy() && castType.type->isIntegerTy())
    {
        return CastIntToInt(value, castType);
    }
    // float to float
    else if (value.type.type->isFloatingPointTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastFloatToFloat(value, castType);
    }
    // float to int
    else if (value.type.type->isFloatingPointTy() &&
             castType.type->isIntegerTy())
    {
        return CastFloatToInt(value, castType);
    }
    else if (value.type.type->isIntegerTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastIntToFloat(value, castType);
    }
    else if (value.type.type->isPointerTy() && castType.type->isPointerTy())
    {
        return CastPtrToPtr(value, castType);
    }
    else if (value.type.type->isIntegerTy() && castType.type->isPointerTy())
    {
        return CastIntToPtr(value, castType);
    }
    else if (value.type.type->isPointerTy() && castType.type->isIntegerTy())
    {
        return CastPtrToInt(value, castType);
    }
    else
    {
        return IRValue{};
    }
    // int to float
    // pointer casting stuff later.
}

IRValue CodeGen::GetLValue(Expression *expr)
{
    IRValue lValue = {};
    if (!expr->IsLValue())
    {
        ReportError(std::format("Expression '{}' is not l-value",
                                expr->GetCodeErrString().str),
                    expr->GetCodeErrString());
    }
    if (dynamic_cast<PrimaryExpression *>(expr))
    {
        auto pe = dynamic_cast<PrimaryExpression *>(expr);
        IRVariable *var = GetIRVariable(pe->GetPrimary()->GetLexeme());
        lValue.value = var->aInsta;
        lValue.type = var->irType;
    }
    else if (dynamic_cast<UnaryExpression *>(expr))
    {
        lValue = GenExpressionIR(expr);
    }
    return lValue;
}

CodeGen::CodeGen(SemanticAnalyzer &semanticAnalyzer, std::string targetArch)
    : semanticAnalyzer(semanticAnalyzer)
{
    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("ztoon module", *ctx);
    module->setTargetTriple(targetArch);
    moduleDataLayout = std::make_unique<llvm::DataLayout>(module.get());
    irBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
}
CodeGen::~CodeGen() {}
void CodeGen::GenIR()
{

    for (Statement *statement : semanticAnalyzer.statements)
    {
        GenStatementIR(statement);
    }
}
void CodeGen::GenStatementIR(Statement *statement)
{

    if (dynamic_cast<VarDeclStatement *>(statement))
    {
        VarDeclStatement *varDeclStatement =
            dynamic_cast<VarDeclStatement *>(statement);
        // Define variable. stack variable.
        llvm::AllocaInst *inst = irBuilder->CreateAlloca(
            ZtoonTypeToLLVMType(
                semanticAnalyzer.stmtToDataTypeMap[varDeclStatement])
                .type,
            nullptr, varDeclStatement->GetIdentifier()->GetLexeme());
        IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
        irVariable->aInsta = inst;
        irVariable->variabel = semanticAnalyzer.currentScope->GetVariable(
            varDeclStatement->GetIdentifier()->GetLexeme(),
            varDeclStatement->GetCodeErrString());
        irVariable->irType = ZtoonTypeToLLVMType(
            semanticAnalyzer.stmtToDataTypeMap[varDeclStatement]);
        AddIRVariable(irVariable);

        if (varDeclStatement->GetExpression())
        {
            // evaluate expression;
            IRValue value = GenExpressionIR(varDeclStatement->GetExpression());
            irBuilder->CreateStore(value.value, inst);
        }
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {
        // Need to check if ptr or variable.
        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);

        IRValue lValue = GetLValue(varAssignmentStatement->GetLValue());

        // Assign value to predefined variable.
        IRValue rValue = GenExpressionIR(varAssignmentStatement->GetRValue());

        irBuilder->CreateStore(rValue.value, lValue.value);
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {
        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);
        IRValue lValue = GetLValue(varComAssignStatement->GetLValue());

        // Assign value to predefined variable.
        IRValue rValue = GenExpressionIR(varComAssignStatement->GetRValue());

        irBuilder->CreateStore(rValue.value, lValue.value);
    }
    else if (dynamic_cast<ExpressionStatement *>(statement))
    {
        ExpressionStatement *exprStatement =
            dynamic_cast<ExpressionStatement *>(statement);
        // just evaluate the expression.
        IRValue ignore = GenExpressionIR(exprStatement->GetExpression());
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {
        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);
        Scope *temp = semanticAnalyzer.currentScope;
        semanticAnalyzer.currentScope =
            semanticAnalyzer.blockToScopeMap[blockStatement];
        for (Statement *s : blockStatement->GetStatements())
        {
            GenStatementIR(s);
        }
        semanticAnalyzer.currentScope = temp;
    }
    else if (dynamic_cast<IfStatement *>(statement))
    {
        IfStatement *ifStatement = dynamic_cast<IfStatement *>(statement);
        IRValue expression = GenExpressionIR(ifStatement->GetExpression());

        llvm::BasicBlock *currentBlock = irBuilder->GetInsertBlock();
        llvm::Function *currentFunction = currentBlock->getParent();

        llvm::BasicBlock *trueBlock =
            llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
        llvm::BasicBlock *falseBlock =
            llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
        llvm::BasicBlock *mergeBlock =
            llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);
        irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(ifStatement->GetBlockStatement());
        llvm::Instruction *term = irBuilder->GetInsertBlock()->getTerminator();

        if (!term || !llvm::isa<llvm::ReturnInst>(term))
        {
            irBuilder->CreateBr(mergeBlock);
        }

        IfStatementData ifData = {};
        ifData.falseBlock = falseBlock;
        ifData.mergeBlock = mergeBlock;
        ifData.currentFunction = currentFunction;
        if (ifStatement->GetNextElseIforElseStatements().size() != 0)
        {
            for (Statement *s : ifStatement->GetNextElseIforElseStatements())
            {
                GenIfStatementIR(s, &ifData);
            }
        }
        else
        {
            llvm::Instruction *term =
                irBuilder->GetInsertBlock()->getTerminator();

            if (!term || !llvm::isa<llvm::ReturnInst>(term))
            {
                irBuilder->SetInsertPoint(falseBlock);
                irBuilder->CreateBr(mergeBlock);
            }
        }
        if (ifData.falseBlock)
        {
            if (!term || !llvm::isa<llvm::ReturnInst>(term))
            {
                irBuilder->SetInsertPoint(ifData.falseBlock);
                irBuilder->CreateBr(mergeBlock);
            }
        }

        irBuilder->SetInsertPoint(mergeBlock);
    }
    else if (dynamic_cast<WhileLoopStatement *>(statement))
    {
        WhileLoopStatement *whileStatement =
            dynamic_cast<WhileLoopStatement *>(statement);
        llvm::Function *currentFunction =
            irBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *loopBlock =
            llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
        llvm::BasicBlock *exitBlock =
            llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);

        IRValue cond = GenExpressionIR(whileStatement->GetCondition());
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

        irBuilder->SetInsertPoint(loopBlock);
        GenStatementIR(whileStatement->GetBlockStatement());
        cond = GenExpressionIR(whileStatement->GetCondition());
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

        irBuilder->SetInsertPoint(exitBlock);
    }
    else if (dynamic_cast<ForLoopStatement *>(statement))
    {
        ForLoopStatement *forLoopStatement =
            dynamic_cast<ForLoopStatement *>(statement);
        GenStatementIR(forLoopStatement->GetInit());
        IRValue cond = GenExpressionIR(forLoopStatement->GetCondition());
        llvm::Function *currentFunction =
            irBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *loopBlock =
            llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
        llvm::BasicBlock *exitBlock =
            llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);

        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

        irBuilder->SetInsertPoint(loopBlock);
        GenStatementIR(forLoopStatement->GetBlockStatement());
        cond = GenExpressionIR(forLoopStatement->GetCondition());
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

        irBuilder->SetInsertPoint(exitBlock);
    }
    else if (dynamic_cast<FnStatement *>(statement))
    {
        auto *fnStmt = dynamic_cast<FnStatement *>(statement);
        Function *fn = semanticAnalyzer.currentScope->GetFunction(
            fnStmt->GetIdentifier()->GetLexeme(), fnStmt->GetCodeErrString());

        FnPointerDataType *fnDataType = fn->fnPointer;

        llvm::Type *retType =
            ZtoonTypeToLLVMType(fnDataType->returnDataType).type;

        std::vector<llvm::Type *> fnParams;

        for (DataType *param : fnDataType->GetParameters())
        {
            fnParams.push_back(ZtoonTypeToLLVMType(param).type);
        }

        llvm::FunctionType *fnType =
            llvm::FunctionType::get(retType, fnParams, fnStmt->IsVarArgs());

        llvm::Function *function = llvm::Function::Create(
            fnType, llvm::GlobalValue::ExternalLinkage, fn->GetName(), *module);

        IRFunction *irFunc = gZtoonArena.Allocate<IRFunction>();
        irFunc->fn = function;
        irFunc->fnType = fnType;
        irFunc->ztoonFn = fn;
        AddIRFunction(irFunc);

        if (!fnStmt->IsPrototype())
        {
            llvm::BasicBlock *fnBB = llvm::BasicBlock::Create(
                *ctx, std::format("{}FnBlock", fn->GetName()), irFunc->fn);
            irFunc->fnBB = fnBB;
            irBuilder->SetInsertPoint(fnBB);

            size_t index = 0;
            for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
            {
                GenStatementIR(paramStmt);
                IRVariable *paramVar =
                    GetIRVariable(paramStmt->GetIdentifier()->GetLexeme());
                llvm::Value *value = irFunc->fn->getArg(index);
                irBuilder->CreateStore(value, paramVar->aInsta);
                index++;
            }
            GenStatementIR(fnStmt->GetBlockStatement());

            llvm::Instruction *term =
                irBuilder->GetInsertBlock()->getTerminator();

            if (!term || !llvm::isa<llvm::ReturnInst>(term))
            {
                if (irFunc->ztoonFn->fnPointer->returnDataType->type !=
                    DataType::Type::NOTYPE)
                {
                    irBuilder->CreateRet(
                        GenExpressionIR(
                            irFunc->ztoonFn->retStmt->GetExpression())
                            .value);
                }
                else
                {
                    irBuilder->CreateRetVoid();
                }
            }
        }
    }
    else if (dynamic_cast<RetStatement *>(statement))
    {
        auto *retStmt = dynamic_cast<RetStatement *>(statement);
        IRFunction *fn = irFunctionsMaps
            [retStmt->GetFnStatement()->GetIdentifier()->GetFilename()];

        if (semanticAnalyzer.stmtToDataTypeMap[retStmt]->GetType() !=
            DataType::Type::NOTYPE)
        {

            IRValue retValue = GenExpressionIR(retStmt->GetExpression());

            irBuilder->CreateRet(retValue.value);
        }
        else
        {
            irBuilder->CreateRetVoid();
        }
    }
}

void CodeGen::GenIfStatementIR(Statement *statement, IfStatementData *ifData)
{
    /*
        if true {

        }
        else if true {

        }
        else if true {

        }
        else {

        }

        br cnd, ifTrueBlock, ifFalseBlock

        ifTrueBlock:
            ....
            br mergeBlock
        ifFalseBlock:
            br elif_cnd, elifTrueBlock, elifFalseBlock
        elifTrueBLock:
            ....
            br mergeBlock
        elifFalseBlock:
            ; maybe another elif or else or nothing
            br cnd, elifTrueBlock2, elifFalseBlock2
        elifTrueBlock2:
            ...
            br mergeBlock
        elIfFalseBlock3:
            br elseBlock
        elseBlock:
            ....
            br mergeBlock
        mergeBlock:
        ... cont.
    */
    if (dynamic_cast<ElseIfStatement *>(statement))
    {
        ElseIfStatement *elifStatement =
            dynamic_cast<ElseIfStatement *>(statement);
        assert(ifData != nullptr);

        irBuilder->SetInsertPoint(ifData->falseBlock);

        IRValue expression = GenExpressionIR(elifStatement->GetExpression());
        llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
            *ctx, "trueBlock", ifData->currentFunction);
        llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(
            *ctx, "falseBlock", ifData->currentFunction);
        irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(elifStatement->GetBlockStatement());

        llvm::Instruction *term = irBuilder->GetInsertBlock()->getTerminator();

        if (!term || !llvm::isa<llvm::ReturnInst>(term))
        {
            irBuilder->CreateBr(ifData->mergeBlock);
        }

        // Prepare for next else if or else
        ifData->falseBlock = falseBlock;
    }
    else if (dynamic_cast<ElseStatement *>(statement))
    {
        ElseStatement *elseStatement = dynamic_cast<ElseStatement *>(statement);
        assert(ifData != nullptr);

        llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
            *ctx, "trueBlock", ifData->currentFunction);
        irBuilder->SetInsertPoint(ifData->falseBlock);
        irBuilder->CreateBr(trueBlock);

        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(elseStatement->GetBlockStatement());

        llvm::Instruction *term = irBuilder->GetInsertBlock()->getTerminator();

        if (!term || !llvm::isa<llvm::ReturnInst>(term))
        {
            irBuilder->CreateBr(ifData->mergeBlock);
        }

        // exit
        ifData->falseBlock = nullptr;
    }
}

bool CodeGen::IsNaN(llvm::Value *value)
{
    if (value->getType()->isFloatingPointTy())
    {
        llvm::ConstantFP *fp = llvm::dyn_cast<llvm::ConstantFP>(value);
        if (fp)
        {
            return fp->isNaN();
        }
    }
    return false;
}
llvm::CmpInst::Predicate
CodeGen::CMPZtoonTypeToCMPLLVM(TokenType type, IRValue value, bool isNaN)
{
    bool isSigned = value.type.isSigned;
    bool isInteger = value.type.type->isIntegerTy();
    switch (type)
    {
    case TokenType::EQUAL_EQUAL:
    {
        if (isInteger)
        {
            return llvm::CmpInst::Predicate::ICMP_EQ;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UEQ
                         : llvm::CmpInst::Predicate::FCMP_OEQ;
        }
    }
    case TokenType::EXCLAMATION_EQUAL:
    {
        if (isInteger)
        {
            return llvm::CmpInst::Predicate::ICMP_NE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UNE
                         : llvm::CmpInst::Predicate::FCMP_ONE;
        }
    }
    case TokenType::LESS:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SLT
                            : llvm::CmpInst::Predicate::ICMP_ULT;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_ULT
                         : llvm::CmpInst::Predicate::FCMP_OLT;
        }
    }
    case TokenType::LESS_EQUAL:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SLE
                            : llvm::CmpInst::Predicate::ICMP_ULE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_ULE
                         : llvm::CmpInst::Predicate::FCMP_OLE;
        }
    }
    case TokenType::GREATER:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SGT
                            : llvm::CmpInst::Predicate::ICMP_UGT;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UGT
                         : llvm::CmpInst::Predicate::FCMP_OGT;
        }
    }
    case TokenType::GREATER_EQUAL:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SGE
                            : llvm::CmpInst::Predicate::ICMP_UGE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UGE
                         : llvm::CmpInst::Predicate::FCMP_OGE;
        }
    }
    default:
        return llvm::CmpInst::Predicate::FCMP_FALSE;
    }
}
IRValue CodeGen::GenExpressionIR(Expression *expression)
{
    IRValue irValue = {};
    if (dynamic_cast<FnCallExpression *>(expression))
    {
        auto *fnCallExpr = dynamic_cast<FnCallExpression *>(expression);

        IRFunction *fn =
            irFunctionsMaps[fnCallExpr->GetIdentifier()->GetLexeme()];

        std::vector<llvm::Value *> args;

        for (auto *arg : fnCallExpr->GetArgs())
        {
            args.push_back(GenExpressionIR(arg).value);
        }

        irValue.type =
            ZtoonTypeToLLVMType(semanticAnalyzer.exprToDataTypeMap[fnCallExpr]);
        irValue.value = irBuilder->CreateCall(
            fn->fn, args, std::format("{}_call", fn->ztoonFn->GetName()));
    }
    else if (dynamic_cast<TernaryExpression *>(expression))
    {
        TernaryExpression *ternaryExpr =
            dynamic_cast<TernaryExpression *>(expression);

        IRValue condition = GenExpressionIR(ternaryExpr->GetCondition());

        llvm::Function *currentFunction =
            irBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *trueBlock =
            llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
        llvm::BasicBlock *falseBlock =
            llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
        llvm::BasicBlock *mergeBlock =
            llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);

        irBuilder->CreateCondBr(condition.value, trueBlock, falseBlock);

        irBuilder->SetInsertPoint(trueBlock);

        IRValue trueValue = GenExpressionIR(ternaryExpr->GetTrueExpression());
        irBuilder->CreateBr(mergeBlock);

        irBuilder->SetInsertPoint(falseBlock);

        IRValue falseValue = GenExpressionIR(ternaryExpr->GetFalseExpression());

        irBuilder->CreateBr(mergeBlock);

        irBuilder->SetInsertPoint(mergeBlock);
        llvm::PHINode *phi =
            irBuilder->CreatePHI(trueValue.type.type, 2, "result");
        phi->addIncoming(trueValue.value, trueBlock);
        phi->addIncoming(falseValue.value, falseBlock);
        irValue.value = phi;
        irValue.type = trueValue.type;
    }
    else if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        IRValue lValue = GenExpressionIR(binaryExpression->GetLeftExpression());
        IRValue rValue =
            GenExpressionIR(binaryExpression->GetRightExpression());
        DataType *binExprDataType =
            semanticAnalyzer.exprToDataTypeMap[binaryExpression];
        DataType *rightExprDataType =
            semanticAnalyzer
                .exprToDataTypeMap[binaryExpression->GetRightExpression()];
        DataType *leftExprDataType =
            semanticAnalyzer
                .exprToDataTypeMap[binaryExpression->GetLeftExpression()];
        irValue.type = ZtoonTypeToLLVMType(binExprDataType);
        switch (binaryExpression->GetOperator()->GetType())
        {
        case TokenType::PLUS:
        {
            if (binExprDataType->IsInteger())
            {
                irValue.value = irBuilder->CreateAdd(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (binExprDataType->IsFloat())
            {

                irValue.value = irBuilder->CreateFAdd(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }

            break;
        }
        case TokenType::DASH:
        {
            if (binExprDataType->IsInteger())
            {
                irValue.value = irBuilder->CreateSub(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (binExprDataType->IsFloat())
            {

                irValue.value = irBuilder->CreateFSub(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::ASTERISK:
        {
            if (binExprDataType->IsInteger())
            {
                irValue.value = irBuilder->CreateMul(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (binExprDataType->IsFloat())
            {

                irValue.value = irBuilder->CreateFMul(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::SLASH:
        {

            if (binExprDataType->IsInteger())
            {
                irValue.value =
                    irValue.type.isSigned
                        ? irBuilder->CreateSDiv(
                              lValue.value, rValue.value,
                              std::format(
                                  "bin_op_{}",
                                  binaryExpression->GetOperator()->GetLexeme()))
                        : irBuilder->CreateUDiv(
                              lValue.value, rValue.value,
                              std::format("bin_op_{}",
                                          binaryExpression->GetOperator()
                                              ->GetLexeme()));
            }
            else if (binExprDataType->IsFloat())
            {

                irValue.value = irBuilder->CreateFDiv(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::PERCENTAGE:
        {

            if (binExprDataType->IsInteger())
            {
                irValue.value =
                    lValue.type.isSigned
                        ? irBuilder->CreateSRem(
                              lValue.value, rValue.value,
                              std::format(
                                  "bin_op_{}",
                                  binaryExpression->GetOperator()->GetLexeme()))
                        : irBuilder->CreateURem(
                              lValue.value, rValue.value,
                              std::format("bin_op_{}",
                                          binaryExpression->GetOperator()
                                              ->GetLexeme()));
            }
            else if (binExprDataType->IsFloat())
            {
                irValue.value = irBuilder->CreateFRem(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::BITWISE_AND:
        {
            irValue.value = irBuilder->CreateAnd(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::BITWISE_OR:
        {
            irValue.value = irBuilder->CreateOr(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::BITWISE_XOR:
        {
            irValue.value = irBuilder->CreateXor(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::SHIFT_LEFT:
        {

            irValue.value = irBuilder->CreateShl(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::SHIFT_RIGHT:
        {
            irValue.value =
                lValue.type.isSigned
                    ? irBuilder->CreateAShr(
                          lValue.value, rValue.value,
                          std::format(
                              "bin_op_{}",
                              binaryExpression->GetOperator()->GetLexeme()))
                    : irBuilder->CreateLShr(
                          lValue.value, rValue.value,
                          std::format(
                              "bin_op_{}",
                              binaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::EQUAL_EQUAL:
        case TokenType::EXCLAMATION_EQUAL:
        case TokenType::LESS:
        case TokenType::LESS_EQUAL:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUAL:
        {
            if (!leftExprDataType->IsNumerical() &&
                leftExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Left expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                leftExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else if (!rightExprDataType->IsNumerical() &&
                     rightExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Right expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                rightExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else
            {
                irValue.value = irBuilder->CreateCmp(
                    CMPZtoonTypeToCMPLLVM(
                        binaryExpression->GetOperator()->GetType(), lValue,
                        IsNaN(lValue.value) || IsNaN(rValue.value)),
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));

                break;
            }
        }
        case TokenType::OR:
        {
            if (!leftExprDataType->IsNumerical() &&
                leftExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Left expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                leftExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else if (!!rightExprDataType->IsNumerical() &&
                     rightExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Right expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                rightExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else
            {
                irValue.value = irBuilder->CreateOr(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
                break;
            }
            break;
        }
        case TokenType::AND:
        {
            if (!leftExprDataType->IsNumerical() &&
                leftExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Left expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                leftExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else if (!!rightExprDataType->IsNumerical() &&
                     rightExprDataType->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Right expression of binary operator '{}' "
                                "cannot be of type '{}'",
                                binaryExpression->GetOperator()->GetLexeme(),
                                rightExprDataType->ToString()),
                    binaryExpression->GetCodeErrString());
            }
            else
            {
                irValue.value = irBuilder->CreateAnd(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
                break;
            }
            break;
        }
        default:
        {
            ReportError(
                std::format("Binary operator '{}' is not supported.",
                            binaryExpression->GetOperator()->GetLexeme()),
                binaryExpression->GetCodeErrString());
            break;
        }
        }
    }
    else if (dynamic_cast<UnaryExpression *>(expression))
    {
        UnaryExpression *unaryExpression =
            dynamic_cast<UnaryExpression *>(expression);
        IRValue rValue = GenExpressionIR(unaryExpression->GetRightExpression());
        irValue.type = rValue.type;
        DataType *unaryDataType =
            semanticAnalyzer.exprToDataTypeMap[unaryExpression];
        switch (unaryExpression->GetOperator()->GetType())
        {
        case TokenType::DASH:
        {
            irValue.type.isSigned = !irValue.type.isSigned;
            if (unaryDataType->IsInteger())
            {

                irValue.value = irBuilder->CreateNeg(
                    rValue.value,
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (unaryDataType->IsFloat())
            {
                irValue.value = irBuilder->CreateFNeg(
                    rValue.value,
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::TILDE:
        {
            if (unaryDataType->IsInteger())
            {
                irValue.value = irBuilder->CreateXor(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), -1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (unaryDataType->IsFloat())
            {
                irValue.value = irBuilder->CreateXor(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), -1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::DASH_DASH:
        {
            PrimaryExpression *primaryExpr = dynamic_cast<PrimaryExpression *>(
                unaryExpression->GetRightExpression());
            IRVariable *var =
                GetIRVariable(primaryExpr->GetPrimary()->GetLexeme());
            if (unaryDataType->IsInteger())
            {
                // Get the variable

                irValue.value = irBuilder->CreateSub(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (unaryDataType->IsFloat())
            {
                irValue.value = irBuilder->CreateFSub(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            irBuilder->CreateStore(irValue.value, var->aInsta);
            break;
        }
        case TokenType::PLUS:
        {
            irValue = rValue;
            break;
        }
        case TokenType::PLUS_PLUS:
        {

            PrimaryExpression *primaryExpr = dynamic_cast<PrimaryExpression *>(
                unaryExpression->GetRightExpression());
            IRVariable *var =
                GetIRVariable(primaryExpr->GetPrimary()->GetLexeme());
            irValue.value = irBuilder->CreateLoad(var->irType.type, var->aInsta,
                                                  var->variabel->GetName());
            if (unaryDataType->IsInteger())
            {

                irValue.value = irBuilder->CreateAdd(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (unaryDataType->IsFloat())
            {
                irValue.value = irBuilder->CreateFAdd(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            irBuilder->CreateStore(irValue.value, var->aInsta);
            break;
        }
        case TokenType::EXCLAMATION:
        {
            // numerical
            irValue.value = irBuilder->CreateXor(
                rValue.value, llvm::ConstantInt::getTrue(*ctx),
                std::format("unary_op_{}",
                            unaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::SIZEOF:
        {
            irValue.type = ZtoonTypeToLLVMType(
                semanticAnalyzer.currentScope->datatypesMap["u64"]);
            uint64_t size =
                moduleDataLayout->getTypeAllocSize(rValue.value->getType());
            irValue.value =
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), size);
            break;
        }
        case TokenType::ASTERISK:
        {
            PointerDataType *ptr = dynamic_cast<PointerDataType *>(
                semanticAnalyzer
                    .exprToDataTypeMap[unaryExpression->GetRightExpression()]);
            irValue.type = ZtoonTypeToLLVMType(ptr->dataType);
            irValue.value = irBuilder->CreateLoad(
                irValue.type.type, rValue.value,
                std::format("deref_{}", unaryExpression->GetRightExpression()
                                            ->GetCodeErrString()
                                            .str));
            break;
        }
        case TokenType::BITWISE_AND:
        {
            // need to get ptr. i just need its vluae
            irValue = GetLValue(unaryExpression->GetRightExpression());
            break;
        }
        default:
        {
            ReportError(
                std::format("Unkown unary operator '{}'",
                            unaryExpression->GetOperator()->GetLexeme()),
                unaryExpression->GetCodeErrString());
            break;
        }
        }
    }
    else if (dynamic_cast<GroupingExpression *>(expression))
    {
        GroupingExpression *groupingExpression =
            dynamic_cast<GroupingExpression *>(expression);

        irValue = GenExpressionIR(groupingExpression->GetExpression());
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        DataType *castDataType =
            semanticAnalyzer.exprToDataTypeMap[castExpression];
        IRValue toCastValue = GenExpressionIR(castExpression->GetExpression());
        IRType castType = ZtoonTypeToLLVMType(castDataType);
        IRType originalType = ZtoonTypeToLLVMType(
            semanticAnalyzer
                .exprToDataTypeMap[castExpression->GetExpression()]);
        irValue = CastIRValue(toCastValue, castType);
    }
    else if (dynamic_cast<PrimaryExpression *>(expression))
    {
        PrimaryExpression *primaryExpression =
            dynamic_cast<PrimaryExpression *>(expression);
        DataType *primaryDataType =
            semanticAnalyzer.exprToDataTypeMap[primaryExpression];
        irValue.type = ZtoonTypeToLLVMType(primaryDataType);
        switch (primaryExpression->GetPrimary()->GetType())
        {
        case TokenType::INTEGER_LITERAL:
        {
            TokenLiteral<int32_t> const *literal =
                dynamic_cast<TokenLiteral<int32_t> const *>(
                    primaryExpression->GetPrimary());
            irValue.value =
                llvm::ConstantInt::get(irValue.type.type, literal->GetValue());
            break;
        }
        case TokenType::FLOAT_LITERAL:
        {
            TokenLiteral<float> const *literal =
                dynamic_cast<TokenLiteral<float> const *>(
                    primaryExpression->GetPrimary());
            irValue.value =
                llvm::ConstantFP::get(irValue.type.type, literal->GetValue());
            break;
        }

        case TokenType::CHARACTER_LITERAL:
        {
            TokenLiteral<int8_t> const *literal =
                dynamic_cast<TokenLiteral<int8_t> const *>(
                    primaryExpression->GetPrimary());
            irValue.value =
                llvm::ConstantInt::get(irValue.type.type, literal->GetValue());
            break;
        }
        case TokenType::TRUE:
        {
            irValue.value = llvm::ConstantInt::getTrue(*ctx);
            break;
        }
        case TokenType::FALSE:
        {
            irValue.value = llvm::ConstantInt::getFalse(*ctx);
            break;
        }
        case TokenType::IDENTIFIER:
        {
            IRVariable *var =
                GetIRVariable(primaryExpression->GetPrimary()->GetLexeme());
            irValue.value = irBuilder->CreateLoad(var->irType.type, var->aInsta,
                                                  "load_var_value");
        }
        break;
        case TokenType::STRING_LITERAL:
        default:
        {
            ReportError(
                std::format("This primary type '{}'  is not supported",
                            primaryExpression->GetPrimary()->GetLexeme()),
                primaryExpression->GetCodeErrString());
            break;
        }
        }
    }
    else
    {
        ReportError("This expression is not supported.",
                    expression->GetCodeErrString());
    };

    return irValue;
}
