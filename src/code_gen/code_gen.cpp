#include "code_gen.h"
#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Casting.h"
#include <cstddef>
#include <format>
#include <memory>
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
    {
        auto structZtoonType = dynamic_cast<StructDataType *>(type);

        llvm::StructType *structIRType =
            llvm::StructType::getTypeByName(*ctx, structZtoonType->name);
        if (!structIRType)
        {
            structIRType =
                llvm::StructType::create(*ctx, structZtoonType->name);
            std::vector<llvm::Type *> bodyFields;
            for (DataType *fieldType : structZtoonType->fields)
            {
                IRType irFieldType = ZtoonTypeToLLVMType(fieldType);
                bodyFields.push_back(irFieldType.type);
            }
            structIRType->setBody(bodyFields);
        }

        irType.type = structIRType;
    }
    break;
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
    case DataType::Type::ARRAY:
    {
        auto arrZtoonType = dynamic_cast<ArrayDataType *>(type);
        auto dataType = arrZtoonType->dataType;
        IRType irDataType = ZtoonTypeToLLVMType(dataType);
        if (arrZtoonType->sizeExpr)
        {
            IRValue arrSize = GenExpressionIR(arrZtoonType->sizeExpr);
            if (!llvm::isa<llvm::Constant>(arrSize.value))
            {
                ReportError("Array size must be constant",
                            arrZtoonType->sizeExpr->GetCodeErrString());
            }
            int64_t arrConstSize =
                llvm::dyn_cast<llvm::ConstantInt>(arrSize.value)
                    ->getSExtValue();
            if (arrConstSize < 0)
            {
                ReportError("Array size must be positive intiger",
                            arrZtoonType->sizeExpr->GetCodeErrString());
            }
            else if (arrConstSize == 0)
            {
                ReportError("Array size cannot be zero",
                            arrZtoonType->sizeExpr->GetCodeErrString());
            }
            irType.type = llvm::ArrayType::get(irDataType.type, arrConstSize);
            arrZtoonType->size = arrConstSize;
        }
        else
        {
            irType.type =
                llvm::ArrayType::get(irDataType.type, arrZtoonType->size);
        }
    }
    break;
    case DataType::Type::FN:
    {
        auto fnPtr = dynamic_cast<FnDataType *>(type);
        llvm::Type *retType = ZtoonTypeToLLVMType(fnPtr->returnDataType).type;

        std::vector<llvm::Type *> fnParams;

        for (DataType *param : fnPtr->GetParameters())
        {
            fnParams.push_back(ZtoonTypeToLLVMType(param).type);
        }

        llvm::FunctionType *fnType =
            llvm::FunctionType::get(retType, fnParams, fnPtr->IsVarArgs());
        irType.type = fnType;
    }
    break;
    default:
    {
        break;
    }
    }

    return irType;
}

void CodeGen::AddIRSymbol(IRSymbol *irSymbol)
{
    assert(irSymbol);
    if (!scopeToIRSymbolsMap.contains(semanticAnalyzer.currentScope))
    {
        scopeToIRSymbolsMap[semanticAnalyzer.currentScope] = {};
    }

    (scopeToIRSymbolsMap[semanticAnalyzer.currentScope])[irSymbol->GetName()] =
        irSymbol;
}

IRSymbol *CodeGen::GetIRSymbol(std::string name)
{
    IRSymbol *ret = nullptr;
    Scope const *scope = semanticAnalyzer.currentScope;
    while (!ret)
    {
        if ((scopeToIRSymbolsMap[scope]).contains(name))
        {
            IRSymbol *ret = (scopeToIRSymbolsMap[scope])[name];
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
    if (value.value->getType()->isIntegerTy() && castType.type->isIntegerTy())
    {
        return CastIntToInt(value, castType);
    }
    // float to float
    else if (value.value->getType()->isFloatingPointTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastFloatToFloat(value, castType);
    }
    // float to int
    else if (value.value->getType()->isFloatingPointTy() &&
             castType.type->isIntegerTy())
    {
        return CastFloatToInt(value, castType);
    }
    else if (value.value->getType()->isIntegerTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastIntToFloat(value, castType);
    }
    // ptr to ptr
    else if (value.value->getType()->isPointerTy() &&
             castType.type->isPointerTy())
    {
        return CastPtrToPtr(value, castType);
    }
    // int to ptr
    else if (value.value->getType()->isIntegerTy() &&
             castType.type->isPointerTy())
    {
        return CastIntToPtr(value, castType);
    }
    // ptr to int
    else if (value.value->getType()->isPointerTy() &&
             castType.type->isIntegerTy())
    {
        return CastPtrToInt(value, castType);
    }
    else
    {
        return IRValue{};
    }
}

CodeGen::CodeGen(SemanticAnalyzer &semanticAnalyzer, std::string targetArch)
    : semanticAnalyzer(semanticAnalyzer)
{
    currentStage = Stage::CODE_GEN;
    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("ztoon module", *ctx);
    module->setTargetTriple(targetArch);
    moduleDataLayout = std::make_unique<llvm::DataLayout>(module.get());
    irBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
}

llvm::Constant *
CodeGen::InitListToArrayConstant(ArrayDataType *arrType,
                                 InitializerListExpression *listExpr)
{
    std::vector<llvm::Constant *> consts;
    for (Expression *expr : listExpr->GetExpressions())
    {
        auto le = dynamic_cast<InitializerListExpression *>(expr);
        IRValue value;
        if (le && arrType->dataType->GetType() == DataType::Type::ARRAY)
        {
            auto type = dynamic_cast<ArrayDataType *>(arrType->dataType);
            value.value = InitListToArrayConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else if (le && arrType->dataType->GetType() == DataType::Type::STRUCT)
        {
            auto type = dynamic_cast<StructDataType *>(arrType->dataType);
            value.value = InitListToStructConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else
        {
            value = GenExpressionIR(expr);
        }
        llvm::Constant *valueConst =
            llvm::dyn_cast<llvm::Constant>(value.value);
        if (!valueConst)
        {
            ReportError("Expression is not constant", expr->GetCodeErrString());
        }
        consts.push_back(valueConst);
    }

    return llvm::ConstantArray::get(
        llvm::dyn_cast<llvm::ArrayType>(ZtoonTypeToLLVMType(arrType).type),
        consts);
}
llvm::Constant *
CodeGen::InitListToStructConstant(StructDataType *structType,
                                  InitializerListExpression *listExpr)
{
    std::vector<llvm::Constant *> consts;
    if (structType->fields.size() != listExpr->GetExpressions().size())
    {
        ReportError(
            std::format("List expression does not match struct '{}' size",
                        structType->GetName()),
            listExpr->GetCodeErrString());
    }
    for (size_t i = 0; i < structType->fields.size(); i++)
    {
        auto expr = listExpr->GetExpressions()[i];
        auto fieldType = structType->fields[i];
        auto le = dynamic_cast<InitializerListExpression *>(expr);
        IRValue value;
        if (expr && le && fieldType->GetType() == DataType::Type::ARRAY)
        {
            auto type = dynamic_cast<ArrayDataType *>(fieldType);
            value.value = InitListToArrayConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else if (expr && le && fieldType->GetType() == DataType::Type::STRUCT)
        {
            auto type = dynamic_cast<StructDataType *>(fieldType);
            value.value = InitListToStructConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else
        {
            if (expr)
            {
                value = GenExpressionIR(expr);
            }
            else
            {
                value.value = llvm::Constant::getNullValue(
                    ZtoonTypeToLLVMType(fieldType).type);
            }
        }
        llvm::Constant *valueConst =
            llvm::dyn_cast<llvm::Constant>(value.value);
        if (!valueConst)
        {
            ReportError("Expression is not constant", expr->GetCodeErrString());
        }
        consts.push_back(valueConst);
    }

    return llvm::ConstantStruct::get(
        llvm::dyn_cast<llvm::StructType>(ZtoonTypeToLLVMType(structType).type),
        consts);
}
void CodeGen::AssignValueToVarArray(IRValue ptr, Expression *expr,
                                    ArrayDataType *arrType,
                                    std::vector<llvm::Value *> &index)

{
    auto initListExpr = dynamic_cast<InitializerListExpression *>(expr);
    auto rValueArrType =
        dynamic_cast<ArrayDataType *>(semanticAnalyzer.exprToDataTypeMap[expr]);
    if (initListExpr)
    {
        if (initListExpr->GetExpressions().empty())
        {
            llvm::Value *GEP = nullptr;
            if (ptr.type.type->isArrayTy())
            {
                GEP = irBuilder->CreateInBoundsGEP(
                    ptr.type.type, ptr.value,
                    {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            }
            else
            {
                GEP = ptr.value;
            }
            // copy data
            IRType elementType = ZtoonTypeToLLVMType(arrType->dataType);
            size_t arraySizeInBytes =
                arrType->size * (elementType.type->getScalarSizeInBits() / 8);
            llvm::Value *sizeValue = irBuilder->getInt64(arraySizeInBytes);
            llvm::Align alignment(elementType.type->getScalarSizeInBits() / 8);
            llvm::Value *zeroValue = llvm::Constant::getNullValue(
                ZtoonTypeToLLVMType(arrType->dataType).type);
            GEP = irBuilder->CreatePointerCast(
                GEP, llvm::Type::getInt8Ty(*ctx)->getPointerTo());
            irBuilder->CreateMemSet(
                GEP, llvm::ConstantInt::get(llvm::Type::getInt8Ty(*ctx), 0),
                sizeValue, alignment);
            return;
        }
        else if (arrType->size != initListExpr->GetExpressions().size())
        {
            ReportError("Initializer list size does not match array size",
                        initListExpr->GetCodeErrString());
        }

        auto innerType = dynamic_cast<ArrayDataType *>(arrType->dataType);
        IRType arrIRType = ZtoonTypeToLLVMType(arrType);
        if (ptr.type.type->isArrayTy() && index.size() == 0)
        {
            index.push_back(irBuilder->getInt32(0));
        }
        index.push_back(irBuilder->getInt32(0));
        size_t i = 0;
        for (auto elementExpr : initListExpr->GetExpressions())
        {
            index.back() = irBuilder->getInt32(i);
            i++;
            if (innerType)
            {
                AssignValueToVarArray(ptr, elementExpr, innerType, index);
            }
            else
            {
                IRValue value = GenExpressionIR(elementExpr);

                llvm::Value *GEP = irBuilder->CreateInBoundsGEP(
                    ptr.type.type, ptr.value, index,
                    std::format("element_{}", i));
                if (dynamic_cast<StructDataType *>(arrType->dataType) &&
                    elementExpr)
                {
                    auto structType =
                        dynamic_cast<StructDataType *>(arrType->dataType);
                    AssignValueToVarStruct({GEP, {}}, elementExpr, structType);
                }
                else
                {
                    irBuilder->CreateStore(value.value, GEP);
                }
            }
        }
        index.pop_back();
    }
    else if (rValueArrType)
    {
        if (rValueArrType->ToString() != arrType->ToString())
        {
            ReportError(std::format("Type {} and type {} are not compatible",
                                    arrType->ToString(),
                                    rValueArrType->ToString()),
                        expr->GetCodeErrString());
        }
        // copy data
        IRType elementType = ZtoonTypeToLLVMType(rValueArrType->dataType);
        size_t arraySizeInBytes =
            rValueArrType->size * (elementType.type->getScalarSizeInBits() / 8);
        llvm::Value *sizeValue = irBuilder->getInt64(arraySizeInBytes);
        llvm::Align alignment(elementType.type->getScalarSizeInBits() / 8);
        IRValue src = GenExpressionIR(expr);
        if (src.type.type->isArrayTy())
        {
            llvm::Value *arrAlloca =
                irBuilder->CreateAlloca(ZtoonTypeToLLVMType(rValueArrType).type,
                                        nullptr, expr->GetCodeErrString().str);
            irBuilder->CreateStore(src.value, arrAlloca);
            src.value = irBuilder->CreateInBoundsGEP(
                ZtoonTypeToLLVMType(rValueArrType).type, arrAlloca,
                {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            src.type.type = ptr.value->getType();
        }
        IRValue dest = ptr;
        irBuilder->CreateMemCpy(dest.value, alignment, src.value, alignment,
                                sizeValue);
    }
    else
    {
        // error
        ReportError(
            std::format(
                "Cannot assign expression of type '{}' to array type '{}'",
                semanticAnalyzer.exprToDataTypeMap[expr]->ToString(),
                arrType->ToString()),
            expr->GetCodeErrString());
    }
}

void CodeGen::AssignValueToVarStruct(IRValue ptr, Expression *expr,
                                     StructDataType *structType)
{
    auto exprListType =
        dynamic_cast<InitListType *>(semanticAnalyzer.exprToDataTypeMap[expr]);
    auto exprStructType = dynamic_cast<StructDataType *>(
        semanticAnalyzer.exprToDataTypeMap[expr]);
    IRType irStructType = ZtoonTypeToLLVMType(structType);
    if (exprListType)
    {
        auto listExpr = dynamic_cast<InitializerListExpression *>(expr);

        if (listExpr->GetExpressions().size() != structType->fields.size())
        {
            ReportError(
                std::format("List expression does not match struct '{}' size",
                            structType->GetName()),
                listExpr->GetCodeErrString());
        }

        for (size_t index = 0; index < listExpr->GetExpressions().size();
             index++)
        {

            llvm::Value *fieldPtr =
                irBuilder->CreateStructGEP(irStructType.type, ptr.value, index);
            auto listElement = listExpr->GetExpressions()[index];
            auto fieldType = structType->fields[index];

            if (fieldType->GetType() == DataType::Type::STRUCT && listElement)
            {
                AssignValueToVarStruct(
                    {fieldPtr, {false, fieldPtr->getType()}}, expr,
                    dynamic_cast<StructDataType *>(fieldType));
            }
            else if (fieldType->GetType() == DataType::Type::ARRAY &&
                     listElement)
            {
                std::vector<llvm::Value *> indices;
                AssignValueToVarArray(
                    {fieldPtr, {false, fieldPtr->getType()}}, listElement,
                    dynamic_cast<ArrayDataType *>(fieldType), indices);
            }
            else
            {
                IRValue fieldValue;
                if (listElement)
                {
                    fieldValue = GenExpressionIR(listElement);
                }
                else
                {
                    fieldValue.value = llvm::Constant::getNullValue(
                        ZtoonTypeToLLVMType(fieldType).type);
                }

                irBuilder->CreateStore(fieldValue.value, fieldPtr);
            }
        }
    }
    else if (exprStructType)
    {

        if (exprStructType->ToString() != structType->ToString())
        {
            ReportError(std::format("Type {} and type {} are not compatible",
                                    structType->ToString(),
                                    exprStructType->ToString()),
                        expr->GetCodeErrString());
        }
        // copy data
        size_t structSizeInBytes =
            moduleDataLayout->getTypeAllocSize(irStructType.type);
        llvm::Value *sizeValue = irBuilder->getInt64(structSizeInBytes);
        llvm::Align alignment(
            moduleDataLayout->getABITypeAlign(irStructType.type));
        IRValue src = GenExpressionIR(expr);

        IRValue dest = ptr;
        irBuilder->CreateMemCpy(dest.value, alignment, src.value, alignment,
                                sizeValue);
    }
    else
    {
        // error
        ReportError(
            std::format(
                "Cannot assign expression of type '{}' to struct type '{}'",
                semanticAnalyzer.exprToDataTypeMap[expr]->ToString(),
                structType->ToString()),
            expr->GetCodeErrString());
    }
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
        DataType *type = semanticAnalyzer.stmtToDataTypeMap[varDeclStatement];
        ArrayDataType *arrType = dynamic_cast<ArrayDataType *>(type);
        StructDataType *structType = dynamic_cast<StructDataType *>(type);
        IRType varDeclType = {};
        varDeclType.type = ZtoonTypeToLLVMType(type).type;
        varDeclType.isSigned = type->IsSigned();
        if (varDeclStatement->IsGlobal())
        {
            llvm::GlobalVariable *globalVar;
            if (varDeclStatement->GetExpression())
            {
                if (arrType || structType)
                {
                    auto listExpr = dynamic_cast<InitializerListExpression *>(
                        varDeclStatement->GetExpression());
                    if (!listExpr)
                    {
                        ReportError(
                            std::format(
                                "Global {} variables can only be initialized "
                                "with a list of constant expressions",
                                arrType ? "array" : "struct"),
                            varDeclStatement->GetCodeErrString());
                    }
                    llvm::Constant *consts =
                        arrType
                            ? InitListToArrayConstant(arrType, listExpr)
                            : InitListToStructConstant(structType, listExpr);

                    globalVar = new llvm::GlobalVariable(
                        *module, varDeclType.type, type->IsReadOnly(),
                        llvm::GlobalValue::ExternalLinkage, consts,
                        std::format(
                            "g_{}",
                            varDeclStatement->GetIdentifier()->GetLexeme()));
                }

                else
                {
                    auto pe = dynamic_cast<PrimaryExpression *>(
                        varDeclStatement->GetExpression());
                    bool isExprGlobalVar = pe ? pe->GetPrimary()->GetType() ==
                                                    TokenType::IDENTIFIER
                                              : false;
                    IRValue exprValue =
                        GenExpressionIR(varDeclStatement->GetExpression());
                    if (!llvm::isa<llvm::Constant>(exprValue.value) ||
                        type->GetType() == DataType::Type::FN)
                    {
                        ReportError(
                            std::format(
                                "Expression '{}' is not known at compile time",
                                varDeclStatement->GetExpression()
                                    ->GetCodeErrString()
                                    .str),
                            varDeclStatement->GetExpression()
                                ->GetCodeErrString());
                    }
                    llvm::Constant *constValue =
                        llvm::cast<llvm::Constant>(exprValue.value);
                    globalVar = new llvm::GlobalVariable(
                        *module, varDeclType.type, type->IsReadOnly(),
                        llvm::GlobalValue::ExternalLinkage,
                        isExprGlobalVar
                            ? llvm::cast<llvm::GlobalVariable>(exprValue.value)
                                  ->getInitializer()
                            : constValue,
                        std::format(
                            "g_{}",
                            varDeclStatement->GetIdentifier()->GetLexeme()));
                }
            }
            else
            {
                globalVar = new llvm::GlobalVariable(
                    *module, varDeclType.type, type->IsReadOnly(),
                    llvm::GlobalValue::ExternalLinkage,
                    structType ? InitListToStructConstant(
                                     structType, structType->defaultValuesList)
                               : llvm::Constant::getNullValue(varDeclType.type),
                    std::format(
                        "g_{}",
                        varDeclStatement->GetIdentifier()->GetLexeme()));
            }

            IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
            irVariable->value = globalVar;
            irVariable->variabel = dynamic_cast<Variable *>(
                semanticAnalyzer.currentScope->GetSymbol(
                    varDeclStatement->GetIdentifier()->GetLexeme(),
                    varDeclStatement->GetCodeErrString()));
            irVariable->irType = varDeclType;
            AddIRSymbol(irVariable);
        }
        else
        {

            llvm::AllocaInst *inst = irBuilder->CreateAlloca(
                varDeclType.type, nullptr,
                varDeclStatement->GetIdentifier()->GetLexeme());
            IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
            irVariable->value = inst;
            irVariable->variabel = dynamic_cast<Variable *>(
                semanticAnalyzer.currentScope->GetSymbol(
                    varDeclStatement->GetIdentifier()->GetLexeme(),
                    varDeclStatement->GetCodeErrString()));
            irVariable->irType = varDeclType;
            AddIRSymbol(irVariable);

            if (varDeclStatement->GetExpression())
            {
                if (arrType)
                {
                    std::vector<llvm::Value *> index;
                    AssignValueToVarArray(
                        {irVariable->value, irVariable->irType},
                        varDeclStatement->GetExpression(), arrType, index);
                }
                else if (structType)
                {
                    AssignValueToVarStruct(
                        {irVariable->value, irVariable->irType},
                        varDeclStatement->GetExpression(), structType);
                }
                else
                {
                    IRValue value =
                        GenExpressionIR(varDeclStatement->GetExpression());
                    irBuilder->CreateStore(value.value, inst);
                }
            }
            else
            {
                if (structType)
                {
                    AssignValueToVarStruct({inst, irVariable->irType},
                                           structType->defaultValuesList,
                                           structType);
                }
                else
                {
                    irBuilder->CreateStore(
                        llvm::Constant::getNullValue(
                            ZtoonTypeToLLVMType(irVariable->variabel->dataType)
                                .type),
                        inst);
                }
            }
        }
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {
        // Need to check if ptr or variable.
        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);

        IRValue lValue =
            GenExpressionIR(varAssignmentStatement->GetLValue(), true);

        // Assign value to predefined variable.
        auto arrType = dynamic_cast<ArrayDataType *>(
            semanticAnalyzer
                .exprToDataTypeMap[varAssignmentStatement->GetLValue()]);
        auto structType = dynamic_cast<StructDataType *>(
            semanticAnalyzer
                .exprToDataTypeMap[varAssignmentStatement->GetLValue()]);
        if (arrType)
        {
            std::vector<llvm::Value *> index;
            AssignValueToVarArray(
                {lValue.value, ZtoonTypeToLLVMType(arrType->dataType)},
                varAssignmentStatement->GetRValue(), arrType, index);
        }
        else if (structType)
        {
            AssignValueToVarStruct({lValue.value, {}},
                                   varAssignmentStatement->GetRValue(),
                                   structType);
        }
        else
        {
            IRValue rValue =
                GenExpressionIR(varAssignmentStatement->GetRValue());
            irBuilder->CreateStore(rValue.value, lValue.value);
        }
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {
        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);
        IRValue lValue =
            GenExpressionIR(varComAssignStatement->GetLValue(), true);

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
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
        }
        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(ifStatement->GetBlockStatement());

        if (!irBuilder->GetInsertBlock()->getTerminator())
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
            irBuilder->SetInsertPoint(falseBlock);
            if (!irBuilder->GetInsertBlock()->getTerminator())
            {
                irBuilder->CreateBr(mergeBlock);
            }
        }
        if (ifData.falseBlock)
        {

            irBuilder->SetInsertPoint(ifData.falseBlock);
            if (!irBuilder->GetInsertBlock()->getTerminator())
            {
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
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
        }

        irBuilder->SetInsertPoint(loopBlock);

        IRLoop *temp = currentLoop;
        currentLoop = gZtoonArena.Allocate<IRLoop>();
        currentLoop->loopBB = loopBlock;
        currentLoop->extBB = exitBlock;
        currentLoop->loopStmt = whileStatement;

        GenStatementIR(whileStatement->GetBlockStatement());
        cond = GenExpressionIR(whileStatement->GetCondition());
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
        }
        irBuilder->SetInsertPoint(exitBlock);
        currentLoop = temp;
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
        llvm::BasicBlock *condBlock =
            llvm::BasicBlock::Create(*ctx, "condBlock", currentFunction);
        llvm::BasicBlock *exitBlock =
            llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

        irBuilder->SetInsertPoint(loopBlock);
        IRLoop *temp = currentLoop;
        currentLoop = gZtoonArena.Allocate<IRLoop>();
        currentLoop->loopBB = loopBlock;
        currentLoop->extBB = exitBlock;
        currentLoop->condBB = condBlock;
        currentLoop->loopStmt = forLoopStatement;
        GenStatementIR(forLoopStatement->GetBlockStatement());
        // in case of continue we need to do this cond.
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(condBlock);
        }
        irBuilder->SetInsertPoint(condBlock);
        // update
        GenStatementIR(forLoopStatement->GetUpdate());
        cond = GenExpressionIR(forLoopStatement->GetCondition());
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
        }
        irBuilder->SetInsertPoint(exitBlock);
        currentLoop = temp;
    }
    else if (dynamic_cast<BreakStatement *>(statement))
    {
        auto bStmt = dynamic_cast<BreakStatement *>(statement);
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(currentLoop->extBB);
        }
    }
    else if (dynamic_cast<ContinueStatement *>(statement))
    {
        auto cStmt = dynamic_cast<ContinueStatement *>(statement);
        // condBB for 'for loop' and exitBB for 'while loop'
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(currentLoop->condBB ? currentLoop->condBB
                                                    : currentLoop->loopBB);
        }
    }
    else if (dynamic_cast<FnStatement *>(statement))
    {
        auto *fnStmt = dynamic_cast<FnStatement *>(statement);
        Function *fn =
            dynamic_cast<Function *>(semanticAnalyzer.currentScope->GetSymbol(
                fnStmt->GetIdentifier()->GetLexeme(),
                fnStmt->GetCodeErrString()));

        FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();
        llvm::FunctionType *fnType = llvm::dyn_cast<llvm::FunctionType>(
            ZtoonTypeToLLVMType(fnDataType).type);
        llvm::Function *function = llvm::Function::Create(
            fnType, llvm::GlobalValue::ExternalLinkage, fn->GetName(), *module);

        IRFunction *irFunc = gZtoonArena.Allocate<IRFunction>();
        irFunc->fn = function;
        irFunc->fnType = fnType;
        irFunc->ztoonFn = fn;
        AddIRSymbol(irFunc);

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
                IRVariable *paramVar = dynamic_cast<IRVariable *>(
                    GetIRSymbol(paramStmt->GetIdentifier()->GetLexeme()));
                llvm::Value *value = irFunc->fn->getArg(index);
                irBuilder->CreateStore(value, paramVar->value);
                index++;
            }
            GenStatementIR(fnStmt->GetBlockStatement());

            llvm::Instruction *term =
                irBuilder->GetInsertBlock()->getTerminator();

            if (!term || !llvm::isa<llvm::ReturnInst>(term))
            {
                if (irFunc->ztoonFn->GetFnDataTypeFromFnPTR()
                        ->returnDataType->type != DataType::Type::NOTYPE)
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
        IRFunction *fn = dynamic_cast<IRFunction *>(GetIRSymbol(
            retStmt->GetFnStatement()->GetIdentifier()->GetLexeme()));

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

        if (!irBuilder->GetInsertBlock()->getTerminator())
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

        if (!irBuilder->GetInsertBlock()->getTerminator())
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
IRValue CodeGen::GenExpressionIR(Expression *expression, bool isWrite)
{
    IRValue irValue = {};
    if (dynamic_cast<FnCallExpression *>(expression))
    {
        auto *fnCallExpr = dynamic_cast<FnCallExpression *>(expression);

        IRValue exprValue = GenExpressionIR(fnCallExpr->GetGetExpression());

        auto fnPtrType = dynamic_cast<PointerDataType *>(
            semanticAnalyzer.exprToDataTypeMap[fnCallExpr->GetGetExpression()]);
        auto fnType =
            dynamic_cast<FnDataType *>(fnPtrType->PointedToDatatype());
        std::vector<llvm::Value *> args;
        size_t index = 0;
        for (auto *arg : fnCallExpr->GetArgs())
        {
            IRValue argIrValue = GenExpressionIR(arg);

            ArrayDataType *paramArrayType;
            if (index < fnType->GetParameters().size())
            {
                paramArrayType = dynamic_cast<ArrayDataType *>(
                    fnType->GetParameters()[index]);
            }
            else
            {
                if (fnType->IsVarArgs())
                {
                    paramArrayType = dynamic_cast<ArrayDataType *>(
                        semanticAnalyzer.exprToDataTypeMap[arg]);
                }
                else
                {
                    ReportError(
                        "Arguments are not compatible with function parameters",
                        fnCallExpr->GetCodeErrString());
                }
            }

            // Copy array "pass by value"
            if (paramArrayType)
            {
                llvm::Value *arrAlloca = irBuilder->CreateAlloca(
                    ZtoonTypeToLLVMType(paramArrayType).type, nullptr,
                    arg->GetCodeErrString().str);

                std::vector<llvm::Value *> index;
                index.push_back(irBuilder->getInt32(0));
                AssignValueToVarArray(
                    {arrAlloca, ZtoonTypeToLLVMType(paramArrayType->dataType)},
                    arg, paramArrayType, index);
                argIrValue.value = irBuilder->CreateLoad(
                    ZtoonTypeToLLVMType(paramArrayType).type, arrAlloca);
            }

            args.push_back(argIrValue.value);

            index++;
        }
        // Get fnPointerDatatype
        auto retDataType = dynamic_cast<DataType *>(
            semanticAnalyzer.exprToDataTypeMap[fnCallExpr]);
        IRType fnIrType = ZtoonTypeToLLVMType(fnType);
        llvm::FunctionType *fnIrTypeCast =
            llvm::cast<llvm::FunctionType>(fnIrType.type);
        irValue.type = ZtoonTypeToLLVMType(retDataType);
        irValue.value = irBuilder->CreateCall(
            fnIrTypeCast, exprValue.value, args,
            retDataType->type == DataType::Type::NOTYPE
                ? ""
                : std::format(
                      "{}_call",
                      fnCallExpr->GetGetExpression()->GetCodeErrString().str));
    }
    else if (dynamic_cast<MemberAccessExpression *>(expression))
    {
        MemberAccessExpression *maExpr =
            dynamic_cast<MemberAccessExpression *>(expression);
        auto type =
            semanticAnalyzer.exprToDataTypeMap[maExpr->GetLeftExpression()];
        StructDataType *structType = dynamic_cast<StructDataType *>(type);
        PointerDataType *ptrType = dynamic_cast<PointerDataType *>(type);
        IRValue leftValue =
            GenExpressionIR(maExpr->GetLeftExpression(), !ptrType);
        if (ptrType)
        {
            structType = dynamic_cast<StructDataType *>(ptrType->dataType);
        }
        size_t index = 0;
        PrimaryExpression *primaryExpr =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());
        std::string rightName = primaryExpr->GetPrimary()->GetLexeme();
        IRType fieldIRType = {};
        for (size_t i = 0; i < structType->structStmt->GetFields().size(); i++)
        {
            if (rightName == structType->structStmt->GetFields()[i]
                                 ->GetIdentifier()
                                 ->GetLexeme())
            {
                index = i;
                fieldIRType = ZtoonTypeToLLVMType(structType->fields[index]);
                break;
            }
        }
        IRType structIRType = ZtoonTypeToLLVMType(structType);
        irValue.value = irBuilder->CreateStructGEP(structIRType.type,
                                                   leftValue.value, index);
        irValue.type = fieldIRType;

        if (!isWrite)
        {
            irValue.value =
                irBuilder->CreateLoad(fieldIRType.type, irValue.value);
        }
    }
    else if (dynamic_cast<SubscriptExpression *>(expression))
    {
        SubscriptExpression *subExpr =
            dynamic_cast<SubscriptExpression *>(expression);

        auto exprDataType = dynamic_cast<ArrayDataType *>(
            semanticAnalyzer.exprToDataTypeMap[subExpr->GetExpression()]);

        IRValue ptr = GenExpressionIR(subExpr->GetExpression(), exprDataType);
        IRValue index = GenExpressionIR(subExpr->GetIndexExpression());
        irValue.type =
            ZtoonTypeToLLVMType(semanticAnalyzer.exprToDataTypeMap[subExpr]);
        if (isWrite && ptr.value->getType()->isArrayTy())
        {
            assert(0);
        }
        if (exprDataType && !ptr.value->getType()->isArrayTy())
        {
            ptr.value = irBuilder->CreateInBoundsGEP(
                ZtoonTypeToLLVMType(exprDataType).type, ptr.value,
                {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            ptr.type.type = ptr.value->getType();
        }
        else if (ptr.value->getType()->isArrayTy())
        {
            llvm::Value *arrAlloca = irBuilder->CreateAlloca(
                ZtoonTypeToLLVMType(exprDataType).type, nullptr,
                subExpr->GetExpression()->GetCodeErrString().str);
            irBuilder->CreateStore(ptr.value, arrAlloca);
            ptr.value = irBuilder->CreateInBoundsGEP(
                ZtoonTypeToLLVMType(exprDataType).type, arrAlloca,
                {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            ptr.type.type = ptr.value->getType();
        }

        llvm::Value *GEP = irBuilder->CreateInBoundsGEP(
            irValue.type.type, ptr.value, index.value,
            std::format("ptr__{}__at_index__{}",
                        subExpr->GetExpression()->GetCodeErrString().str,
                        subExpr->GetIndexExpression()->GetCodeErrString().str));

        if (isWrite)
        {
            irValue.value = GEP;
        }
        else
        {
            irValue.value = irBuilder->CreateLoad(
                irValue.type.type, GEP,
                std::format(
                    "{}__at_index__{}",
                    subExpr->GetExpression()->GetCodeErrString().str,
                    subExpr->GetIndexExpression()->GetCodeErrString().str));
        }
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
        IRValue rValue;
        if (!unaryExpression->GetSizeOfDataTypeToken())
        {
            rValue = GenExpressionIR(unaryExpression->GetRightExpression());
            irValue.type = rValue.type;
        }

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
            // rvalue here is unary right expression, not meaning the expr
            // is right value expression. my brain.
            IRValue lValue =
                GenExpressionIR(unaryExpression->GetRightExpression());
            rValue.value =
                irBuilder->CreateLoad(lValue.type.type, lValue.value);
            rValue.type = lValue.type;
            irValue.type = rValue.type;
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
            irBuilder->CreateStore(irValue.value, lValue.value);
            break;
        }
        case TokenType::PLUS:
        {
            irValue = rValue;
            break;
        }
        case TokenType::PLUS_PLUS:
        {

            // rvalue here is unary right expression, not meaning the expr
            // is right value expression. my brain.
            IRValue lValue =
                GenExpressionIR(unaryExpression->GetRightExpression());
            rValue.value =
                irBuilder->CreateLoad(lValue.type.type, lValue.value);
            rValue.type = lValue.type;
            irValue.type = rValue.type;
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
            irBuilder->CreateStore(irValue.value, lValue.value);
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
            uint64_t size = moduleDataLayout->getTypeAllocSize(
                rValue.value
                    ? rValue.value->getType()
                    : ZtoonTypeToLLVMType(
                          semanticAnalyzer.currentScope->GetDataType(
                              unaryExpression->GetSizeOfDataTypeToken()))
                          .type);
            irValue.value =
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), size);
            break;
        }
        case TokenType::ASTERISK:
        {
            PointerDataType *ptr = dynamic_cast<PointerDataType *>(
                semanticAnalyzer
                    .exprToDataTypeMap[unaryExpression->GetRightExpression()]);
            if (isWrite)
            {
                irValue = rValue;
            }
            else
            {
                irValue.type = ZtoonTypeToLLVMType(ptr->dataType);
                irValue.value = irBuilder->CreateLoad(
                    irValue.type.type, rValue.value,
                    std::format("deref_{}",
                                unaryExpression->GetRightExpression()
                                    ->GetCodeErrString()
                                    .str));
            }
            break;
        }
        case TokenType::BITWISE_AND:
        {
            irValue =
                GenExpressionIR(unaryExpression->GetRightExpression(), true);
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

        irValue = GenExpressionIR(groupingExpression->GetExpression(), isWrite);
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        DataType *castDataType =
            semanticAnalyzer.exprToDataTypeMap[castExpression];

        IRType castType = ZtoonTypeToLLVMType(castDataType);
        IRType originalType = ZtoonTypeToLLVMType(
            semanticAnalyzer
                .exprToDataTypeMap[castExpression->GetExpression()]);
        bool arrToPtrCast = false;
        if (semanticAnalyzer.exprToDataTypeMap[castExpression->GetExpression()]
                    ->GetType() == DataType::Type::ARRAY &&
            castDataType->GetType() == DataType::Type::POINTER)
        {
            arrToPtrCast = true;
        }
        IRValue toCastValue =
            GenExpressionIR(castExpression->GetExpression(), arrToPtrCast);
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
        case TokenType::NULL_PTR:
        {
            irValue.value = llvm::Constant::getNullValue(irValue.type.type);
            break;
        };
        case TokenType::IDENTIFIER:
        {
            // are we in global scope?
            llvm::BasicBlock *block = irBuilder->GetInsertBlock();

            IRSymbol *symbol =
                GetIRSymbol(primaryExpression->GetPrimary()->GetLexeme());

            IRVariable *var = dynamic_cast<IRVariable *>(symbol);
            if (var)
            {
                if (isWrite || !block)
                {
                    irValue.type.type = symbol->GetType();
                    irValue.value = symbol->GetValue();
                }
                else
                {
                    irValue.value = irBuilder->CreateLoad(symbol->GetType(),
                                                          symbol->GetValue(),
                                                          "load_var_value");
                    irValue.type.type = irValue.value->getType();
                    irValue.type.isSigned = var->irType.isSigned;
                }
            }
            else
            {
                // fn type
                irValue.type.type = symbol->GetType()->getPointerTo();
                irValue.value = symbol->GetValue();
            }
        }
        break;
        case TokenType::STRING_LITERAL:
        {
            TokenLiteral<std::string> const *literal =
                dynamic_cast<TokenLiteral<std::string> const *>(
                    primaryExpression->GetPrimary());
            auto str_const =
                llvm::ConstantDataArray::getString(*ctx, literal->GetValue());
            llvm::GlobalVariable *varInReadOnlySection =
                new llvm::GlobalVariable(*module, str_const->getType(), true,
                                         llvm::GlobalValue::PrivateLinkage,
                                         str_const, "str_ro_section");
            varInReadOnlySection->setSection(".rodata");
            varInReadOnlySection->setAlignment(llvm::Align(1));
            irValue.value = irBuilder->CreateInBoundsGEP(
                varInReadOnlySection->getType(), varInReadOnlySection,
                {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            irValue.type.type = irValue.value->getType();
        }
        break;
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
