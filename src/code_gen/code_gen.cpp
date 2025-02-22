#include "code_gen.h"
#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/TypeSize.h"
#include <format>
IRType CodeGen::TokenTypeToLLVMType(TokenType type)
{
    IRType irType = {};
    switch (type)
    {

    case TokenType::I8:
    {
        irType.type = irBuilder->getIntNTy(8);
        irType.isSigned = true;
        break;
    }
    case TokenType::I16:
    {
        irType.type = irBuilder->getIntNTy(16);
        irType.isSigned = true;
        break;
    }
    case TokenType::I32:
    {
        irType.type = irBuilder->getIntNTy(32);
        irType.isSigned = true;
        break;
    }
    case TokenType::I64:
    {
        irType.type = irBuilder->getIntNTy(64);
        irType.isSigned = true;
        break;
    }
    case TokenType::U8:
    {
        irType.type = irBuilder->getIntNTy(8);
        break;
    }
    case TokenType::U16:
    {
        irType.type = irBuilder->getIntNTy(16);
        break;
    }
    case TokenType::U32:
    {
        irType.type = irBuilder->getIntNTy(32);
        break;
    }
    case TokenType::U64:
    {
        irType.type = irBuilder->getIntNTy(64);
        break;
    }
    case TokenType::F32:
    {
        irType.type = irBuilder->getFloatTy();
        break;
    }
    case TokenType::F64:
    {
        irType.type = irBuilder->getDoubleTy();
        break;
    }
    case TokenType::BOOL:
    {
        irType.type = irBuilder->getIntNTy(1);
        break;
    }
    default:
    {
        ReportError(
            std::format("Type '{}' is not supported by the IR code genrator.",
                        TokenDataTypeToString(type)),
            nullptr);
        break;
    }
    }

    return irType;
}

void CodeGen::AddIRVariable(IRVariable *irVariable)
{
    assert(irVariable);
    irVariablesMap[irVariable->variabel->GetName()] = irVariable;
}

IRVariable *CodeGen::GetIRVariable(std::string name)
{
    // just for checking if variable is accessible here.
    // auto ignore = semanticAnalyzer.currentScope->GetVariable(name);

    if (irVariablesMap.contains(name))
    {
        return irVariablesMap[name];
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
    uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
    uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();
    castedValue.type = castType;
    castedValue.value =
        value.type.isSigned
            ? irBuilder->CreateSIToFP(value.value, castType.type)
            : irBuilder->CreateUIToFP(value.value, castType.type);
    castedValue = CastFloatToFloat(castedValue, castType);
    return castedValue;
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
    else
    {
        ReportError("LLVM backend does not support this casting yet.", nullptr);
        return IRValue{};
    }
    //  int to float
    // pointer casting stuff later.
}

CodeGen::CodeGen(const SemanticAnalyzer &semanticAnalyzer)
    : semanticAnalyzer(semanticAnalyzer)
{
    module = new llvm::Module("ztoon module", ctx);
    moduleDataLayout = new llvm::DataLayout(module);
    irBuilder = new llvm::IRBuilder<>(ctx);
}
CodeGen::~CodeGen()
{
    delete moduleDataLayout;
    delete module;
    delete irBuilder;
}

void CodeGen::GenIR()
{
    for (Statement *statement : semanticAnalyzer.statements)
    {
        if (dynamic_cast<VarDeclStatement *>(statement))
        {
            VarDeclStatement *varDeclStatement =
                dynamic_cast<VarDeclStatement *>(statement);
            // Define variable. stack variable.
            llvm::AllocaInst *inst = irBuilder->CreateAlloca(
                TokenTypeToLLVMType(varDeclStatement->GetDataType()->GetType())
                    .type,
                nullptr, varDeclStatement->GetIdentifier()->GetLexeme());
            IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
            irVariable->aInsta = inst;
            irVariable->variabel = semanticAnalyzer.globalScope.GetVariable(
                varDeclStatement->GetIdentifier()->GetLexeme(),
                varDeclStatement->GetIdentifier());
            irVariable->irType =
                TokenTypeToLLVMType(varDeclStatement->GetDataType()->GetType());
            AddIRVariable(irVariable);

            if (varDeclStatement->GetExpression())
            {
                // evaluate expression;
                IRValue value =
                    GenExpressionIR(varDeclStatement->GetExpression());
                irBuilder->CreateStore(value.value, inst);
            }
        }
        else if (dynamic_cast<VarAssignmentStatement *>(statement))
        {
            VarAssignmentStatement *varAssignmentStatement =
                dynamic_cast<VarAssignmentStatement *>(statement);
            // Assign value to predefined variable.
            IRValue value =
                GenExpressionIR(varAssignmentStatement->GetExpression());
            IRVariable *irVar = GetIRVariable(
                varAssignmentStatement->GetIdentifier()->GetLexeme());
            irBuilder->CreateStore(value.value, irVar->aInsta);
        }
        else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
        {
            VarCompoundAssignmentStatement *varComAssignStatement =
                dynamic_cast<VarCompoundAssignmentStatement *>(statement);
            // similar to assign statement.
            IRValue value =
                GenExpressionIR(varComAssignStatement->GetExpression());
            IRVariable *irVar = GetIRVariable(
                varComAssignStatement->GetIdentifier()->GetLexeme());
            irBuilder->CreateStore(value.value, irVar->aInsta);
        }
        else if (dynamic_cast<ExpressionStatement *>(statement))
        {
            ExpressionStatement *exprStatement =
                dynamic_cast<ExpressionStatement *>(statement);
            // just evaluate the expression.
            IRValue ignore = GenExpressionIR(exprStatement->GetExpression());
        }
    }
}

IRValue CodeGen::GenExpressionIR(Expression *expression)
{
    IRValue irValue = {};

    if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        IRValue lValue = GenExpressionIR(binaryExpression->GetLeftExpression());
        IRValue rValue =
            GenExpressionIR(binaryExpression->GetRightExpression());
        irValue.type = TokenTypeToLLVMType(binaryExpression->GetDataType());
        switch (binaryExpression->GetOperator()->GetType())
        {
        case TokenType::PLUS:
        {
            if (IsInteger(binaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateAdd(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(binaryExpression->GetDataType()))
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
            if (IsInteger(binaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateSub(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(binaryExpression->GetDataType()))
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
            if (IsInteger(binaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateMul(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(binaryExpression->GetDataType()))
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

            if (IsInteger(binaryExpression->GetDataType()))
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
            else if (IsFloat(binaryExpression->GetDataType()))
            {

                irValue.value = irBuilder->CreateFDiv(
                    lValue.value, rValue.value,
                    std::format("bin_op_{}",
                                binaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        default:
        {
            ReportError(
                std::format("Binary operator '{}' is not supported.",
                            binaryExpression->GetOperator()->GetLexeme()),
                binaryExpression->GetOperator());
            break;
        }
        }
    }
    else if (dynamic_cast<UnaryExpression *>(expression))
    {
        UnaryExpression *unaryExpression =
            dynamic_cast<UnaryExpression *>(expression);
        IRValue rValue = GenExpressionIR(unaryExpression->GetRightExpression());
        switch (unaryExpression->GetOperator()->GetType())
        {
        case TokenType::DASH:
        {
            if (IsInteger(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateNeg(
                    rValue.value,
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(unaryExpression->GetDataType()))
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
            if (IsInteger(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateXor(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), -1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(unaryExpression->GetDataType()))
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
            if (IsInteger(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateSub(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateFSub(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::PLUS:
        {
            irValue = rValue;
            break;
        }
        case TokenType::PLUS_PLUS:
        {
            if (IsInteger(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateAdd(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            else if (IsFloat(unaryExpression->GetDataType()))
            {
                irValue.value = irBuilder->CreateFAdd(
                    rValue.value,
                    llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                    std::format("unary_op_{}",
                                unaryExpression->GetOperator()->GetLexeme()));
            }
            break;
        }
        case TokenType::EXCLAMATION:
        {
            // numerical
            irValue.value = irBuilder->CreateXor(
                rValue.value, llvm::ConstantInt::getTrue(ctx),
                std::format("unary_op_{}",
                            unaryExpression->GetOperator()->GetLexeme()));
            break;
        }
        case TokenType::SIZEOF:
        {
            uint64_t size =
                moduleDataLayout->getTypeAllocSize(rValue.value->getType());
            irValue.value =
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx), size);
            break;
        }
        default:
        {
            ReportError(
                std::format("Unkown unary operator '{}'",
                            unaryExpression->GetOperator()->GetLexeme()),
                unaryExpression->GetOperator());
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
        IRValue toCastValue = GenExpressionIR(castExpression->GetExpression());
        IRType castType = TokenTypeToLLVMType(castExpression->GetDataType());
        IRType originalType =
            TokenTypeToLLVMType(castExpression->GetExpression()->GetDataType());
        irValue = CastIRValue(toCastValue, castType);
    }
    else if (dynamic_cast<PrimaryExpression *>(expression))
    {
        PrimaryExpression *primaryExpression =
            dynamic_cast<PrimaryExpression *>(expression);
        irValue.type = TokenTypeToLLVMType(primaryExpression->GetDataType());
        switch (primaryExpression->GetPrimary()->GetType())
        {
        case TokenType::INTEGER_LITERAL:
        {
            TokenLiteral<int32_t> const *literal =
                dynamic_cast<TokenLiteral<int32_t> const *>(
                    primaryExpression->GetPrimary());
            irValue.value = llvm::ConstantInt::get(
                TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
                literal->GetValue());
            break;
        }
        case TokenType::FLOAT_LITERAL:
        {
            TokenLiteral<float> const *literal =
                dynamic_cast<TokenLiteral<float> const *>(
                    primaryExpression->GetPrimary());
            irValue.value = llvm::ConstantFP::get(
                TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
                literal->GetValue());
            break;
        }

        case TokenType::CHARACTER_LITERAL:
        {
            TokenLiteral<int8_t> const *literal =
                dynamic_cast<TokenLiteral<int8_t> const *>(
                    primaryExpression->GetPrimary());
            irValue.value = llvm::ConstantInt::get(
                TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
                literal->GetValue());
            break;
        }
        case TokenType::TRUE:
        {
            irValue.value = llvm::ConstantInt::getTrue(ctx);
            break;
        }
        case TokenType::FALSE:
        {
            irValue.value = llvm::ConstantInt::getFalse(ctx);
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
                primaryExpression->GetPrimary());
            break;
        }
        }
    }
    else
    {
        ReportError("This expression is not supported.", nullptr);
    };

    return irValue;
}
