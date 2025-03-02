// #include "code_gen.h"
// #include "error_report.h"
// #include "lexer/lexer.h"
// #include "parser/parser.h"
// #include "llvm/IR/BasicBlock.h"
// #include "llvm/IR/Constants.h"
// #include "llvm/IR/DataLayout.h"
// #include "llvm/IR/Function.h"
// #include "llvm/IR/IRBuilder.h"
// #include "llvm/IR/InstrTypes.h"
// #include "llvm/IR/Instruction.h"
// #include "llvm/IR/Instructions.h"
// #include "llvm/IR/LLVMContext.h"
// #include "llvm/IR/Type.h"
// #include "llvm/IR/Value.h"
// #include "llvm/Support/Casting.h"
// #include <format>
// #include <memory>
// IRType CodeGen::TokenTypeToLLVMType(TokenType type)
// {
//     IRType irType = {};
//     switch (type)
//     {

//     case TokenType::I8:
//     {
//         irType.type = irBuilder->getIntNTy(8);
//         irType.isSigned = true;
//         break;
//     }
//     case TokenType::I16:
//     {
//         irType.type = irBuilder->getIntNTy(16);
//         irType.isSigned = true;
//         break;
//     }
//     case TokenType::I32:
//     {
//         irType.type = irBuilder->getIntNTy(32);
//         irType.isSigned = true;
//         break;
//     }
//     case TokenType::I64:
//     {
//         irType.type = irBuilder->getIntNTy(64);
//         irType.isSigned = true;
//         break;
//     }
//     case TokenType::U8:
//     {
//         irType.type = irBuilder->getIntNTy(8);
//         break;
//     }
//     case TokenType::U16:
//     {
//         irType.type = irBuilder->getIntNTy(16);
//         break;
//     }
//     case TokenType::U32:
//     {
//         irType.type = irBuilder->getIntNTy(32);
//         break;
//     }
//     case TokenType::U64:
//     {
//         irType.type = irBuilder->getIntNTy(64);
//         break;
//     }
//     case TokenType::F32:
//     {
//         irType.type = irBuilder->getFloatTy();
//         break;
//     }
//     case TokenType::F64:
//     {
//         irType.type = irBuilder->getDoubleTy();
//         break;
//     }
//     case TokenType::BOOL:
//     {
//         irType.type = irBuilder->getIntNTy(1);
//         break;
//     }
//     default:
//     {

//         break;
//     }
//     }

//     return irType;
// }

// void CodeGen::AddIRVariable(IRVariable *irVariable)
// {
//     assert(irVariable);
//     if (!scopeToIRVariablesMap.contains(semanticAnalyzer.currentScope))
//     {
//         scopeToIRVariablesMap[semanticAnalyzer.currentScope] = {};
//     }

//     (scopeToIRVariablesMap[semanticAnalyzer.currentScope])[irVariable->variabel
//                                                                ->GetName()] =
//         irVariable;
// }

// IRVariable *CodeGen::GetIRVariable(std::string name)
// {
//     IRVariable *ret = nullptr;
//     Scope const *scope = semanticAnalyzer.currentScope;
//     while (!ret)
//     {
//         if ((scopeToIRVariablesMap[scope]).contains(name))
//         {
//             IRVariable *ret = (scopeToIRVariablesMap[scope])[name];
//             return ret;
//         }
//         scope = scope->GetParent();
//         if (!scope)
//         {
//             break;
//         }
//     }

//     assert(0);
//     return nullptr;
// }

// IRValue CodeGen::CastIntToInt(IRValue value, IRType castType)
// {
//     IRValue castedValue = {};
//     if ((value.type.type == castType.type) &&
//         (value.type.isSigned == castType.isSigned))
//     {
//         return value;
//     }

//     uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
//     uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();

//     if (valueBitWidth > castTypeBitWidth)
//     {
//         // truncate value
//         castedValue.value = irBuilder->CreateTrunc(value.value,
//         castType.type);
//     }
//     else if (valueBitWidth < castTypeBitWidth)
//     {
//         castedValue.value =
//             value.type.isSigned
//                 ? irBuilder->CreateSExt(value.value, castType.type)
//                 : irBuilder->CreateZExt(value.value, castType.type);
//     }
//     else
//     {
//         castedValue.value = value.value;
//     }

//     castedValue.type = castType;
//     return castedValue;
// }
// IRValue CodeGen::CastFloatToFloat(IRValue value, IRType castType)
// {

//     IRValue castedValue = {};
//     if (value.type.type == castType.type)
//     {
//         return value;
//     }

//     uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
//     uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;

//     if (valueBitWidth > castTypeBitWidth)
//     {
//         // truncate value
//         castedValue.value =
//             irBuilder->CreateFPTrunc(value.value, castType.type);
//     }
//     else
//     {
//         castedValue.value = irBuilder->CreateFPExt(value.value,
//         castType.type);
//     }

//     castedValue.type = castType;
//     return castedValue;
// }
// IRValue CodeGen::CastFloatToInt(IRValue value, IRType castType)
// {
//     IRValue castedValue = {};
//     uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
//     uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();
//     castedValue.type = castType;
//     castedValue.value =
//         castType.isSigned ? irBuilder->CreateFPToSI(value.value,
//         castType.type)
//                           : irBuilder->CreateFPToUI(value.value,
//                           castType.type);
//     castedValue = CastIntToInt(castedValue, castType);
//     return castedValue;
// }
// IRValue CodeGen::CastIntToFloat(IRValue value, IRType castType)
// {

//     IRValue castedValue = {};
//     uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
//     uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;
//     castedValue.type = castType;
//     castedValue.value =
//         value.type.isSigned
//             ? irBuilder->CreateSIToFP(value.value, castType.type)
//             : irBuilder->CreateUIToFP(value.value, castType.type);
//     castedValue = CastFloatToFloat(castedValue, castType);
//     return castedValue;
// }
// IRValue CodeGen::CastIRValue(IRValue value, IRType castType)
// {
//     // int to int
//     if (value.type.type->isIntegerTy() && castType.type->isIntegerTy())
//     {
//         return CastIntToInt(value, castType);
//     }
//     // float to float
//     else if (value.type.type->isFloatingPointTy() &&
//              castType.type->isFloatingPointTy())
//     {
//         return CastFloatToFloat(value, castType);
//     }
//     // float to int
//     else if (value.type.type->isFloatingPointTy() &&
//              castType.type->isIntegerTy())
//     {
//         return CastFloatToInt(value, castType);
//     }
//     else if (value.type.type->isIntegerTy() &&
//              castType.type->isFloatingPointTy())
//     {
//         return CastIntToFloat(value, castType);
//     }
//     else
//     {
//         return IRValue{};
//     }
//     //  int to float
//     // pointer casting stuff later.
// }

// CodeGen::CodeGen(SemanticAnalyzer &semanticAnalyzer)
//     : semanticAnalyzer(semanticAnalyzer)
// {
//     ctx = std::make_unique<llvm::LLVMContext>();
//     module = std::make_unique<llvm::Module>("ztoon module", *ctx);
//     moduleDataLayout = std::make_unique<llvm::DataLayout>(module.get());
//     irBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
// }
// CodeGen::~CodeGen() {}
// void CodeGen::GenIR()
// {

//     for (Statement *statement : semanticAnalyzer.statements)
//     {
//         GenStatementIR(statement);
//     }
// }
// void CodeGen::GenStatementIR(Statement *statement)
// {

//     if (dynamic_cast<VarDeclStatement *>(statement))
//     {
//         VarDeclStatement *varDeclStatement =
//             dynamic_cast<VarDeclStatement *>(statement);
//         // Define variable. stack variable.
//         llvm::AllocaInst *inst = irBuilder->CreateAlloca(
//             TokenTypeToLLVMType(varDeclStatement->GetDataType()->GetType())
//                 .type,
//             nullptr, varDeclStatement->GetIdentifier()->GetLexeme());
//         IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
//         irVariable->aInsta = inst;
//         irVariable->variabel = semanticAnalyzer.currentScope->GetVariable(
//             varDeclStatement->GetIdentifier()->GetLexeme(),
//             varDeclStatement->GetCodeErrString());
//         irVariable->irType =
//             TokenTypeToLLVMType(varDeclStatement->GetDataType()->GetType());
//         AddIRVariable(irVariable);

//         if (varDeclStatement->GetExpression())
//         {
//             // evaluate expression;
//             IRValue value =
//             GenExpressionIR(varDeclStatement->GetExpression());
//             irBuilder->CreateStore(value.value, inst);
//         }
//     }
//     else if (dynamic_cast<VarAssignmentStatement *>(statement))
//     {
//         VarAssignmentStatement *varAssignmentStatement =
//             dynamic_cast<VarAssignmentStatement *>(statement);
//         // Assign value to predefined variable.
//         IRValue value =
//             GenExpressionIR(varAssignmentStatement->GetExpression());
//         IRVariable *irVar =
//             GetIRVariable(varAssignmentStatement->GetIdentifier()->GetLexeme());
//         irBuilder->CreateStore(value.value, irVar->aInsta);
//     }
//     else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
//     {
//         VarCompoundAssignmentStatement *varComAssignStatement =
//             dynamic_cast<VarCompoundAssignmentStatement *>(statement);
//         // similar to assign statement.
//         IRValue value =
//         GenExpressionIR(varComAssignStatement->GetExpression()); IRVariable
//         *irVar =
//             GetIRVariable(varComAssignStatement->GetIdentifier()->GetLexeme());
//         irBuilder->CreateStore(value.value, irVar->aInsta);
//     }
//     else if (dynamic_cast<ExpressionStatement *>(statement))
//     {
//         ExpressionStatement *exprStatement =
//             dynamic_cast<ExpressionStatement *>(statement);
//         // just evaluate the expression.
//         IRValue ignore = GenExpressionIR(exprStatement->GetExpression());
//     }
//     else if (dynamic_cast<BlockStatement *>(statement))
//     {
//         BlockStatement *blockStatement =
//             dynamic_cast<BlockStatement *>(statement);
//         Scope *temp = semanticAnalyzer.currentScope;
//         semanticAnalyzer.currentScope =
//             semanticAnalyzer.blockToScopeMap[blockStatement];
//         for (Statement *s : blockStatement->GetStatements())
//         {
//             GenStatementIR(s);
//         }
//         semanticAnalyzer.currentScope = temp;
//     }
//     else if (dynamic_cast<IfStatement *>(statement))
//     {
//         IfStatement *ifStatement = dynamic_cast<IfStatement *>(statement);
//         IRValue expression = GenExpressionIR(ifStatement->GetExpression());

//         llvm::BasicBlock *currentBlock = irBuilder->GetInsertBlock();
//         llvm::Function *currentFunction = currentBlock->getParent();

//         llvm::BasicBlock *trueBlock =
//             llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
//         llvm::BasicBlock *falseBlock =
//             llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
//         llvm::BasicBlock *mergeBlock =
//             llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);
//         irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
//         irBuilder->SetInsertPoint(trueBlock);

//         GenStatementIR(ifStatement->GetBlockStatement());
//         irBuilder->CreateBr(mergeBlock);

//         IfStatementData ifData = {};
//         ifData.falseBlock = falseBlock;
//         ifData.mergeBlock = mergeBlock;
//         ifData.currentFunction = currentFunction;
//         if (ifStatement->GetNextElseIforElseStatements().size() != 0)
//         {
//             for (Statement *s : ifStatement->GetNextElseIforElseStatements())
//             {
//                 GenIfStatementIR(s, &ifData);
//             }
//         }
//         else
//         {

//             irBuilder->SetInsertPoint(falseBlock);
//             irBuilder->CreateBr(mergeBlock);
//         }
//         if (ifData.falseBlock)
//         {
//             irBuilder->SetInsertPoint(ifData.falseBlock);
//             irBuilder->CreateBr(mergeBlock);
//         }

//         irBuilder->SetInsertPoint(mergeBlock);
//     }
//     else if (dynamic_cast<WhileLoopStatement *>(statement))
//     {
//         WhileLoopStatement *whileStatement =
//             dynamic_cast<WhileLoopStatement *>(statement);
//         llvm::Function *currentFunction =
//             irBuilder->GetInsertBlock()->getParent();
//         llvm::BasicBlock *loopBlock =
//             llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
//         llvm::BasicBlock *exitBlock =
//             llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);

//         IRValue cond = GenExpressionIR(whileStatement->GetCondition());
//         irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

//         irBuilder->SetInsertPoint(loopBlock);
//         GenStatementIR(whileStatement->GetBlockStatement());
//         cond = GenExpressionIR(whileStatement->GetCondition());
//         irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

//         irBuilder->SetInsertPoint(exitBlock);
//     }
//     else if (dynamic_cast<ForLoopStatement *>(statement))
//     {
//         ForLoopStatement *forLoopStatement =
//             dynamic_cast<ForLoopStatement *>(statement);
//         GenStatementIR(forLoopStatement->GetInit());
//         IRValue cond = GenExpressionIR(forLoopStatement->GetCondition());
//         llvm::Function *currentFunction =
//             irBuilder->GetInsertBlock()->getParent();
//         llvm::BasicBlock *loopBlock =
//             llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
//         llvm::BasicBlock *exitBlock =
//             llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);

//         irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

//         irBuilder->SetInsertPoint(loopBlock);
//         GenStatementIR(forLoopStatement->GetBlockStatement());
//         cond = GenExpressionIR(forLoopStatement->GetCondition());
//         irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

//         irBuilder->SetInsertPoint(exitBlock);
//     }
// }

// void CodeGen::GenIfStatementIR(Statement *statement, IfStatementData *ifData)
// {
//     /*
//         if true {

//         }
//         else if true {

//         }
//         else if true {

//         }
//         else {

//         }

//         br cnd, ifTrueBlock, ifFalseBlock

//         ifTrueBlock:
//             ....
//             br mergeBlock
//         ifFalseBlock:
//             br elif_cnd, elifTrueBlock, elifFalseBlock
//         elifTrueBLock:
//             ....
//             br mergeBlock
//         elifFalseBlock:
//             ; maybe another elif or else or nothing
//             br cnd, elifTrueBlock2, elifFalseBlock2
//         elifTrueBlock2:
//             ...
//             br mergeBlock
//         elIfFalseBlock3:
//             br elseBlock
//         elseBlock:
//             ....
//             br mergeBlock
//         mergeBlock:
//         ... cont.
//     */
//     if (dynamic_cast<ElseIfStatement *>(statement))
//     {
//         ElseIfStatement *elifStatement =
//             dynamic_cast<ElseIfStatement *>(statement);
//         assert(ifData != nullptr);

//         irBuilder->SetInsertPoint(ifData->falseBlock);

//         IRValue expression = GenExpressionIR(elifStatement->GetExpression());
//         llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
//             *ctx, "trueBlock", ifData->currentFunction);
//         llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(
//             *ctx, "falseBlock", ifData->currentFunction);
//         irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
//         irBuilder->SetInsertPoint(trueBlock);

//         GenStatementIR(elifStatement->GetBlockStatement());

//         irBuilder->CreateBr(ifData->mergeBlock);

//         // Prepare for next else if or else
//         ifData->falseBlock = falseBlock;
//     }
//     else if (dynamic_cast<ElseStatement *>(statement))
//     {
//         ElseStatement *elseStatement = dynamic_cast<ElseStatement
//         *>(statement); assert(ifData != nullptr);

//         llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
//             *ctx, "trueBlock", ifData->currentFunction);
//         irBuilder->SetInsertPoint(ifData->falseBlock);
//         irBuilder->CreateBr(trueBlock);

//         irBuilder->SetInsertPoint(trueBlock);

//         GenStatementIR(elseStatement->GetBlockStatement());

//         irBuilder->CreateBr(ifData->mergeBlock);

//         // exit
//         ifData->falseBlock = nullptr;
//     }
// }

// bool CodeGen::IsNaN(llvm::Value *value)
// {
//     if (value->getType()->isFloatingPointTy())
//     {
//         llvm::ConstantFP *fp = llvm::dyn_cast<llvm::ConstantFP>(value);
//         if (fp)
//         {
//             return fp->isNaN();
//         }
//     }
//     return false;
// }
// llvm::CmpInst::Predicate CodeGen::CMPTokenToCMPLLVM(TokenType cmpOp,
//                                                     IRValue value, bool
//                                                     isNaN)
// {
//     bool isSigned = value.type.isSigned;
//     bool isInteger = value.type.type->isIntegerTy();
//     switch (cmpOp)
//     {
//     case TokenType::EQUAL_EQUAL:
//     {
//         if (isInteger)
//         {
//             return llvm::CmpInst::Predicate::ICMP_EQ;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_UEQ
//                          : llvm::CmpInst::Predicate::FCMP_OEQ;
//         }
//     }
//     case TokenType::EXCLAMATION_EQUAL:
//     {
//         if (isInteger)
//         {
//             return llvm::CmpInst::Predicate::ICMP_NE;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_UNE
//                          : llvm::CmpInst::Predicate::FCMP_ONE;
//         }
//     }
//     case TokenType::LESS:
//     {
//         if (isInteger)
//         {
//             return isSigned ? llvm::CmpInst::Predicate::ICMP_SLT
//                             : llvm::CmpInst::Predicate::ICMP_ULT;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_ULT
//                          : llvm::CmpInst::Predicate::FCMP_OLT;
//         }
//     }
//     case TokenType::LESS_EQUAL:
//     {
//         if (isInteger)
//         {
//             return isSigned ? llvm::CmpInst::Predicate::ICMP_SLE
//                             : llvm::CmpInst::Predicate::ICMP_ULE;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_ULE
//                          : llvm::CmpInst::Predicate::FCMP_OLE;
//         }
//     }
//     case TokenType::GREATER:
//     {
//         if (isInteger)
//         {
//             return isSigned ? llvm::CmpInst::Predicate::ICMP_SGT
//                             : llvm::CmpInst::Predicate::ICMP_UGT;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_UGT
//                          : llvm::CmpInst::Predicate::FCMP_OGT;
//         }
//     }
//     case TokenType::GREATER_EQUAL:
//     {
//         if (isInteger)
//         {
//             return isSigned ? llvm::CmpInst::Predicate::ICMP_SGE
//                             : llvm::CmpInst::Predicate::ICMP_UGE;
//         }
//         else
//         {
//             return isNaN ? llvm::CmpInst::Predicate::FCMP_UGE
//                          : llvm::CmpInst::Predicate::FCMP_OGE;
//         }
//     }
//     default:
//         return llvm::CmpInst::Predicate::FCMP_FALSE;
//     }
// }
// IRValue CodeGen::GenExpressionIR(Expression *expression)
// {
//     IRValue irValue = {};

//     if (dynamic_cast<TernaryExpression *>(expression))
//     {
//         TernaryExpression *ternaryExpr =
//             dynamic_cast<TernaryExpression *>(expression);

//         IRValue condition = GenExpressionIR(ternaryExpr->GetCondition());

//         llvm::Function *currentFunction =
//             irBuilder->GetInsertBlock()->getParent();
//         llvm::BasicBlock *trueBlock =
//             llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
//         llvm::BasicBlock *falseBlock =
//             llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
//         llvm::BasicBlock *mergeBlock =
//             llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);

//         irBuilder->CreateCondBr(condition.value, trueBlock, falseBlock);

//         irBuilder->SetInsertPoint(trueBlock);

//         IRValue trueValue =
//         GenExpressionIR(ternaryExpr->GetTrueExpression());
//         irBuilder->CreateBr(mergeBlock);

//         irBuilder->SetInsertPoint(falseBlock);

//         IRValue falseValue =
//         GenExpressionIR(ternaryExpr->GetFalseExpression());

//         irBuilder->CreateBr(mergeBlock);

//         irBuilder->SetInsertPoint(mergeBlock);
//         llvm::PHINode *phi =
//             irBuilder->CreatePHI(trueValue.type.type, 2, "result");
//         phi->addIncoming(trueValue.value, trueBlock);
//         phi->addIncoming(falseValue.value, falseBlock);
//         irValue.value = phi;
//         irValue.type = trueValue.type;
//     }
//     else if (dynamic_cast<BinaryExpression *>(expression))
//     {
//         BinaryExpression *binaryExpression =
//             dynamic_cast<BinaryExpression *>(expression);
//         IRValue lValue =
//         GenExpressionIR(binaryExpression->GetLeftExpression()); IRValue
//         rValue =
//             GenExpressionIR(binaryExpression->GetRightExpression());
//         irValue.type = TokenTypeToLLVMType(binaryExpression->GetDataType());
//         switch (binaryExpression->GetOperator()->GetType())
//         {
//         case TokenType::PLUS:
//         {
//             if (IsInteger(binaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateAdd(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(binaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateFAdd(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }

//             break;
//         }
//         case TokenType::DASH:
//         {
//             if (IsInteger(binaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateSub(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(binaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateFSub(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::ASTERISK:
//         {
//             if (IsInteger(binaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateMul(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(binaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateFMul(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::SLASH:
//         {

//             if (IsInteger(binaryExpression->GetDataType()))
//             {
//                 irValue.value =
//                     irValue.type.isSigned
//                         ? irBuilder->CreateSDiv(
//                               lValue.value, rValue.value,
//                               std::format(
//                                   "bin_op_{}",
//                                   binaryExpression->GetOperator()->GetLexeme()))
//                         : irBuilder->CreateUDiv(
//                               lValue.value, rValue.value,
//                               std::format("bin_op_{}",
//                                           binaryExpression->GetOperator()
//                                               ->GetLexeme()));
//             }
//             else if (IsFloat(binaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateFDiv(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::PERCENTAGE:
//         {

//             if (IsInteger(binaryExpression->GetDataType()))
//             {
//                 irValue.value =
//                     lValue.type.isSigned
//                         ? irBuilder->CreateSRem(
//                               lValue.value, rValue.value,
//                               std::format(
//                                   "bin_op_{}",
//                                   binaryExpression->GetOperator()->GetLexeme()))
//                         : irBuilder->CreateURem(
//                               lValue.value, rValue.value,
//                               std::format("bin_op_{}",
//                                           binaryExpression->GetOperator()
//                                               ->GetLexeme()));
//             }
//             else if (IsFloat(binaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateFRem(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::BITWISE_AND:
//         {
//             irValue.value = irBuilder->CreateAnd(
//                 lValue.value, rValue.value,
//                 std::format("bin_op_{}",
//                             binaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::BITWISE_OR:
//         {
//             irValue.value = irBuilder->CreateOr(
//                 lValue.value, rValue.value,
//                 std::format("bin_op_{}",
//                             binaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::BITWISE_XOR:
//         {
//             irValue.value = irBuilder->CreateXor(
//                 lValue.value, rValue.value,
//                 std::format("bin_op_{}",
//                             binaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::SHIFT_LEFT:
//         {

//             irValue.value = irBuilder->CreateShl(
//                 lValue.value, rValue.value,
//                 std::format("bin_op_{}",
//                             binaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::SHIFT_RIGHT:
//         {
//             irValue.value =
//                 lValue.type.isSigned
//                     ? irBuilder->CreateAShr(
//                           lValue.value, rValue.value,
//                           std::format(
//                               "bin_op_{}",
//                               binaryExpression->GetOperator()->GetLexeme()))
//                     : irBuilder->CreateLShr(
//                           lValue.value, rValue.value,
//                           std::format(
//                               "bin_op_{}",
//                               binaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::EQUAL_EQUAL:
//         case TokenType::EXCLAMATION_EQUAL:
//         case TokenType::LESS:
//         case TokenType::LESS_EQUAL:
//         case TokenType::GREATER:
//         case TokenType::GREATER_EQUAL:
//         {
//             if (!IsNumerical(
//                     binaryExpression->GetLeftExpression()->GetDataType()) &&
//                 binaryExpression->GetLeftExpression()->GetDataType() !=
//                     TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Left expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetLeftExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else if (!IsNumerical(binaryExpression->GetRightExpression()
//                                       ->GetDataType()) &&
//                      binaryExpression->GetRightExpression()->GetDataType() !=
//                          TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Right expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetRightExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else
//             {
//                 irValue.value = irBuilder->CreateCmp(
//                     CMPTokenToCMPLLVM(
//                         binaryExpression->GetOperator()->GetType(), lValue,
//                         IsNaN(lValue.value) || IsNaN(rValue.value)),
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));

//                 break;
//             }
//         }
//         case TokenType::OR:
//         {
//             if (!IsNumerical(
//                     binaryExpression->GetLeftExpression()->GetDataType()) &&
//                 binaryExpression->GetLeftExpression()->GetDataType() !=
//                     TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Left expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetLeftExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else if (!IsNumerical(binaryExpression->GetRightExpression()
//                                       ->GetDataType()) &&
//                      binaryExpression->GetRightExpression()->GetDataType() !=
//                          TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Right expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetRightExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else
//             {
//                 irValue.value = irBuilder->CreateOr(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//                 break;
//             }
//             break;
//         }
//         case TokenType::AND:
//         {
//             if (!IsNumerical(
//                     binaryExpression->GetLeftExpression()->GetDataType()) &&
//                 binaryExpression->GetLeftExpression()->GetDataType() !=
//                     TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Left expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetLeftExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else if (!IsNumerical(binaryExpression->GetRightExpression()
//                                       ->GetDataType()) &&
//                      binaryExpression->GetRightExpression()->GetDataType() !=
//                          TokenType::BOOL)
//             {
//                 ReportError(
//                     std::format("Right expression of binary operator '{}' "
//                                 "cannot be of type '{}'",
//                                 binaryExpression->GetOperator()->GetLexeme(),
//                                 TokenDataTypeToString(
//                                     binaryExpression->GetRightExpression()
//                                         ->GetDataType())),
//                     binaryExpression->GetCodeErrString());
//             }
//             else
//             {
//                 irValue.value = irBuilder->CreateAnd(
//                     lValue.value, rValue.value,
//                     std::format("bin_op_{}",
//                                 binaryExpression->GetOperator()->GetLexeme()));
//                 break;
//             }
//             break;
//         }
//         default:
//         {
//             ReportError(
//                 std::format("Binary operator '{}' is not supported.",
//                             binaryExpression->GetOperator()->GetLexeme()),
//                 binaryExpression->GetCodeErrString());
//             break;
//         }
//         }
//     }
//     else if (dynamic_cast<UnaryExpression *>(expression))
//     {
//         UnaryExpression *unaryExpression =
//             dynamic_cast<UnaryExpression *>(expression);
//         IRValue rValue =
//         GenExpressionIR(unaryExpression->GetRightExpression()); irValue.type
//         = rValue.type; switch (unaryExpression->GetOperator()->GetType())
//         {
//         case TokenType::DASH:
//         {
//             irValue.type.isSigned = !irValue.type.isSigned;
//             if (IsInteger(unaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateNeg(
//                     rValue.value,
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(unaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateFNeg(
//                     rValue.value,
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::TILDE:
//         {
//             if (IsInteger(unaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateXor(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), -1),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(unaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateXor(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), -1),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             break;
//         }
//         case TokenType::DASH_DASH:
//         {
//             PrimaryExpression *primaryExpr = dynamic_cast<PrimaryExpression
//             *>(
//                 unaryExpression->GetRightExpression());
//             IRVariable *var =
//                 GetIRVariable(primaryExpr->GetPrimary()->GetLexeme());
//             if (IsInteger(unaryExpression->GetDataType()))
//             {
//                 // Get the variable

//                 irValue.value = irBuilder->CreateSub(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), 1),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(unaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateFSub(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), 1.0),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             irBuilder->CreateStore(irValue.value, var->aInsta);
//             break;
//         }
//         case TokenType::PLUS:
//         {
//             irValue = rValue;
//             break;
//         }
//         case TokenType::PLUS_PLUS:
//         {

//             PrimaryExpression *primaryExpr = dynamic_cast<PrimaryExpression
//             *>(
//                 unaryExpression->GetRightExpression());
//             IRVariable *var =
//                 GetIRVariable(primaryExpr->GetPrimary()->GetLexeme());
//             irValue.value = irBuilder->CreateLoad(var->irType.type,
//             var->aInsta,
//                                                   var->variabel->GetName());
//             if (IsInteger(unaryExpression->GetDataType()))
//             {

//                 irValue.value = irBuilder->CreateAdd(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), 1),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             else if (IsFloat(unaryExpression->GetDataType()))
//             {
//                 irValue.value = irBuilder->CreateFAdd(
//                     rValue.value,
//                     llvm::ConstantInt::get(rValue.value->getType(), 1.0),
//                     std::format("unary_op_{}",
//                                 unaryExpression->GetOperator()->GetLexeme()));
//             }
//             irBuilder->CreateStore(irValue.value, var->aInsta);
//             break;
//         }
//         case TokenType::EXCLAMATION:
//         {
//             // numerical
//             irValue.value = irBuilder->CreateXor(
//                 rValue.value, llvm::ConstantInt::getTrue(*ctx),
//                 std::format("unary_op_{}",
//                             unaryExpression->GetOperator()->GetLexeme()));
//             break;
//         }
//         case TokenType::SIZEOF:
//         {
//             irValue.type = TokenTypeToLLVMType(TokenType::U64);
//             uint64_t size =
//                 moduleDataLayout->getTypeAllocSize(rValue.value->getType());
//             irValue.value =
//                 llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), size);
//             break;
//         }
//         default:
//         {
//             ReportError(
//                 std::format("Unkown unary operator '{}'",
//                             unaryExpression->GetOperator()->GetLexeme()),
//                 unaryExpression->GetCodeErrString());
//             break;
//         }
//         }
//     }
//     else if (dynamic_cast<GroupingExpression *>(expression))
//     {
//         GroupingExpression *groupingExpression =
//             dynamic_cast<GroupingExpression *>(expression);

//         irValue = GenExpressionIR(groupingExpression->GetExpression());
//     }
//     else if (dynamic_cast<CastExpression *>(expression))
//     {
//         CastExpression *castExpression =
//             dynamic_cast<CastExpression *>(expression);
//         IRValue toCastValue =
//         GenExpressionIR(castExpression->GetExpression()); IRType castType =
//         TokenTypeToLLVMType(castExpression->GetDataType()); IRType
//         originalType =
//             TokenTypeToLLVMType(castExpression->GetExpression()->GetDataType());
//         irValue = CastIRValue(toCastValue, castType);
//     }
//     else if (dynamic_cast<PrimaryExpression *>(expression))
//     {
//         PrimaryExpression *primaryExpression =
//             dynamic_cast<PrimaryExpression *>(expression);
//         irValue.type = TokenTypeToLLVMType(primaryExpression->GetDataType());
//         switch (primaryExpression->GetPrimary()->GetType())
//         {
//         case TokenType::INTEGER_LITERAL:
//         {
//             TokenLiteral<int32_t> const *literal =
//                 dynamic_cast<TokenLiteral<int32_t> const *>(
//                     primaryExpression->GetPrimary());
//             irValue.value = llvm::ConstantInt::get(
//                 TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
//                 literal->GetValue());
//             break;
//         }
//         case TokenType::FLOAT_LITERAL:
//         {
//             TokenLiteral<float> const *literal =
//                 dynamic_cast<TokenLiteral<float> const *>(
//                     primaryExpression->GetPrimary());
//             irValue.value = llvm::ConstantFP::get(
//                 TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
//                 literal->GetValue());
//             break;
//         }

//         case TokenType::CHARACTER_LITERAL:
//         {
//             TokenLiteral<int8_t> const *literal =
//                 dynamic_cast<TokenLiteral<int8_t> const *>(
//                     primaryExpression->GetPrimary());
//             irValue.value = llvm::ConstantInt::get(
//                 TokenTypeToLLVMType(primaryExpression->GetDataType()).type,
//                 literal->GetValue());
//             break;
//         }
//         case TokenType::TRUE:
//         {
//             irValue.value = llvm::ConstantInt::getTrue(*ctx);
//             break;
//         }
//         case TokenType::FALSE:
//         {
//             irValue.value = llvm::ConstantInt::getFalse(*ctx);
//             break;
//         }
//         case TokenType::IDENTIFIER:
//         {
//             IRVariable *var =
//                 GetIRVariable(primaryExpression->GetPrimary()->GetLexeme());
//             irValue.value = irBuilder->CreateLoad(var->irType.type,
//             var->aInsta,
//                                                   "load_var_value");
//         }
//         break;
//         case TokenType::STRING_LITERAL:
//         default:
//         {
//             ReportError(
//                 std::format("This primary type '{}'  is not supported",
//                             primaryExpression->GetPrimary()->GetLexeme()),
//                 primaryExpression->GetCodeErrString());
//             break;
//         }
//         }
//     }
//     else
//     {
//         ReportError("This expression is not supported.",
//                     expression->GetCodeErrString());
//     };

//     return irValue;
// }
