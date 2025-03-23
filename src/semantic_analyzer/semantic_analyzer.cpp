#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include "llvm/IR/Intrinsics.h"
#include <cstring>
#include <format>
#include <functional>

std::string DataType::ToString()
{
    std::string str;
    if (isReadOnly)
    {
        str += "readonly ";
    }
    switch (type)
    {
    case DataType::Type::I8:
    {

        str += "i8";
        break;
    }
    case DataType::Type::I16:
    {
        str += "i16";
        break;
    }
    case DataType::Type::I32:
    {
        str += "i32";
        break;
    }
    case DataType::Type::I64:
    {
        str += "i64";
        break;
    }
    case DataType::Type::U8:
    {
        str += "u8";
        break;
    }
    case DataType::Type::U16:
    {
        str += "u16";
        break;
    }
    case DataType::Type::U32:
    {
        str += "u32";
        break;
    }
    case DataType::Type::U64:
    {
        str += "u64";
        break;
    }
    case DataType::Type::F32:
    {
        str += "f32";
        break;
    }
    case DataType::Type::F64:
    {
        str += "f64";
        break;
    }
    case DataType::Type::BOOL:
    {
        str += "bool";
        break;
    }
    case DataType::Type::NOTYPE:
    {
        str += "notype";
        break;
    }
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
    {
        str += AggregateTypeToString();
        break;
    }
    case DataType::Type::POINTER:
    {

        auto ptrType = (PointerDataType *)this;
        str += ptrType->dataType->ToString();
        str += "*";
        break;
    }
    case DataType::Type::ARRAY:
    {
        auto arrType = (ArrayDataType *)this;
        str += arrType->dataType->ToString();
        if (arrType->size != 0)
        {
            str += std::format("[{}]", arrType->size);
        }
        else
        {
            str += "[]";
        }
    }
    break;
    case DataType::Type::InitList:
    {
        auto listType = dynamic_cast<InitListType *>(this);
        str += "{";
        for (auto t : listType->dataTypes)
        {
            str += t->ToString() + ",";
        }
        if (str.ends_with(','))
        {
            str.pop_back();
        }
        str += "}";
    }
    break;
    default:
    {
        str += "Unknown type";
        break;
    }
    }

    return str;
}

std::string FnPointerDataTypeToStringKey(FnPointerDataType *fnDataType)
{

    std::string fpDatatypeStringKey = "fn ";
    for (DataType *paramDatatype : fnDataType->GetParameters())
    {
        fpDatatypeStringKey += std::format("{} ", paramDatatype->ToString());
    }
    fpDatatypeStringKey +=
        std::format("-> {}", fnDataType->GetReturnDataType()->ToString());
    return fpDatatypeStringKey;
}

DataType *Scope::GetDataType(DataTypeToken *dataTypeToken)
{
    std::string typeStr = dataTypeToken->ToString();

    DataType *result = nullptr;
    if (typeStr.empty())
    {
        typeStr = "notype";
    }
    Scope *currentScope = this;
    if (!dataTypeToken->arrayDesc)
    {
        while (currentScope)
        {
            if (currentScope->datatypesMap.contains(typeStr))
            {
                return datatypesMap[typeStr];
            }

            currentScope = currentScope->parent;
        }
    }

    bool dataTypeFound = false;
    currentScope = this;
    while (currentScope)
    {
        if (currentScope->datatypesMap.contains(
                dataTypeToken->GetDataType()->GetLexeme()))
        {
            dataTypeFound = true;
            break;
        }

        currentScope = currentScope->parent;
    }
    if (dataTypeFound)
    {
        if (dataTypeToken->arrayDesc)
        {
            auto arrType = gZtoonArena.Allocate<ArrayDataType>();
            arrType->type = DataType::Type::ARRAY;

            arrType->sizeExpr = dataTypeToken->arrayDesc->arraySizeExpr;
            if (arrType->sizeExpr)
            {
                semanticAnalyzer->EvaluateAndAssignDataTypeToExpression(
                    arrType->sizeExpr);
            }
            arrType->dataType =
                GetDataType(dataTypeToken->arrayDesc->dataTypeToken);
            result = arrType;
        }
        if (dataTypeToken->pointerDesc)
        {

            PointerDataType *ptrDataType =
                gZtoonArena.Allocate<PointerDataType>();

            ptrDataType->type = DataType::Type::POINTER;
            DataTypeToken *token = gZtoonArena.Allocate<DataTypeToken>();
            *token = *(dataTypeToken->pointerDesc->dataTypeToken);
            ptrDataType->dataType = GetDataType(token);

            if (dataTypeToken->readOnly)
            {
                ptrDataType->isReadOnly = true;
            }

            datatypesMap[ptrDataType->ToString()] = ptrDataType;

            result = ptrDataType;
        }
        else if (dataTypeToken->readOnly)
        {
            DataType *type = gZtoonArena.Allocate<DataType>();
            *type = *(datatypesMap[dataTypeToken->GetDataType()->GetLexeme()]);

            type->isReadOnly = true;
            datatypesMap[type->ToString()] = type;
            result = type;
        }
    }
    else
    {
        ReportError("Datatype is not defined.",
                    dataTypeToken->GetCodeErrString());
    }

    return result;
}

Scope::Scope(SemanticAnalyzer *semanticAnalyzer, Scope *parent)
{
    this->parent = parent;
    this->semanticAnalyzer = semanticAnalyzer;
    datatypesMap["i8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i8"]->type = DataType::Type::I8;

    datatypesMap["i16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i16"]->type = DataType::Type::I16;

    datatypesMap["i32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i32"]->type = DataType::Type::I32;

    datatypesMap["i64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i64"]->type = DataType::Type::I64;

    datatypesMap["u8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u8"]->type = DataType::Type::U8;

    datatypesMap["u16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u16"]->type = DataType::Type::U16;

    datatypesMap["u32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u32"]->type = DataType::Type::U32;

    datatypesMap["u64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u64"]->type = DataType::Type::U64;

    datatypesMap["f32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f32"]->type = DataType::Type::F32;
    datatypesMap["f64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f64"]->type = DataType::Type::F64;

    datatypesMap["bool"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["bool"]->type = DataType::Type::BOOL;

    datatypesMap["notype"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["notype"]->type = DataType::Type::NOTYPE;

    auto strPtr = gZtoonArena.Allocate<PointerDataType>();
    strPtr->dataType = datatypesMap["i8"];
    strPtr->type = DataType::Type::POINTER;
    strPtr->isReadOnly = true;
    datatypesMap["readonly i8*"] = strPtr;
}

bool DataType::IsNumerical()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::F32:
    case Type::F64:
        return true;
    default:
        return false;
    }
}
bool DataType::IsInteger()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
        return true;
    default:
        return false;
    }
}
bool DataType::IsFloat()
{
    switch (type)
    {
    case Type::F64:
    case Type::F32:
        return true;
    default:
        return false;
    }
}

bool DataType::IsSigned()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
        return true;
    default:
        return false;
    }
}

TokenType DataType::ToTokenType()
{
    switch (type)
    {
    case DataType::Type::I8:
        return TokenType::I8;
    case DataType::Type::I16:
        return TokenType::I16;
    case DataType::Type::I32:
        return TokenType::I32;
    case DataType::Type::I64:
        return TokenType::I64;
    case DataType::Type::U8:
        return TokenType::U8;
    case DataType::Type::U16:
        return TokenType::U16;
    case DataType::Type::U32:
        return TokenType::U32;
    case DataType::Type::U64:
        return TokenType::U64;
    case DataType::Type::F32:
        return TokenType::F32;
    case DataType::Type::F64:
        return TokenType::F64;
    case DataType::Type::BOOL:
        return TokenType::BOOL;
    case DataType::Type::NOTYPE:
        return TokenType::NOTYPE;
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
        return TokenType::IDENTIFIER;
    default:
        return TokenType::UNKNOWN;
    }
}

Symbol *Scope::GetSymbol(std::string name, CodeErrString codeErrString)
{

    Symbol *symbol = nullptr;

    Scope const *current = this;
    while (!symbol)
    {
        if (current->symbolsMap.contains(name))
        {
            symbol = current->symbolsMap.at(name);
        }
        // go up one level;
        if (!current->parent)
        {
            // no more scopes.
            break;
        }
        current = current->parent;
    }

    if (symbol)
    {
        return symbol;
    }
    else
    {
        ReportError(std::format("'{}' is not defined in this scope.", name),
                    codeErrString);
    }
    return nullptr;
}
void Scope::AddSymbol(Symbol *symbol, CodeErrString codeErrString)
{
    if (symbolsMap.contains(symbol->GetName()))
    {
        Symbol *alreadyDefined = symbolsMap[symbol->GetName()];
        ReportError(std::format("'{}' already defined", symbol->GetName()),
                    codeErrString);
    }
    else
    {
        symbolsMap[symbol->GetName()] = symbol;
    }
}

void SemanticAnalyzer::ValidateAssignValueToVarArray(Expression *expr,
                                                     ArrayDataType *arrType)
{
    auto initListExpr = dynamic_cast<InitializerListExpression *>(expr);
    auto rValueArrType = dynamic_cast<ArrayDataType *>(exprToDataTypeMap[expr]);
    if (initListExpr)
    {
        if (!arrType->sizeExpr)
        {
            arrType->size = initListExpr->GetExpressions().size();
        }
        auto innerType = dynamic_cast<ArrayDataType *>(arrType->dataType);

        for (auto elementExpr : initListExpr->GetExpressions())
        {
            if (innerType)
            {
                // recurse
                ValidateAssignValueToVarArray(elementExpr, innerType);
            }
            else
            {
                auto leftExpr = gZtoonArena.Allocate<PrimaryExpression>();
                leftExpr->primary =
                    gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                leftExpr->isLvalue = true;
                exprToDataTypeMap[leftExpr] = arrType->dataType;
                Expression *e = leftExpr;

                DataType::Type type = DecideDataType(&e, &elementExpr);
                if (type == DataType::Type::UNKNOWN)
                {
                    ReportError(
                        std::format("Types '{}' and '{}' are not compatible",
                                    arrType->dataType->ToString(),
                                    exprToDataTypeMap[elementExpr]->ToString()),
                        elementExpr->GetCodeErrString());
                }
            }
        }
    }
    else if (rValueArrType)
    {
        if (rValueArrType->ToString() != arrType->ToString())
        {
            ReportError(std::format("Types '{}' and '{}' are not compatible",
                                    arrType->ToString(),
                                    rValueArrType->ToString()),
                        expr->GetCodeErrString());
        }
    }
}

SemanticAnalyzer::SemanticAnalyzer(std::vector<Statement *> &statements)
    : statements(statements)
{
    currentScope = gZtoonArena.Allocate<Scope>(this);
}
SemanticAnalyzer::~SemanticAnalyzer() {}
void SemanticAnalyzer::Analize()
{

    for (size_t i = 0; i < statements.size(); i++)
    {
        AnalizeStatement(statements[i]);

        statementCurrentIndex++;
    }
}

void SemanticAnalyzer::PreAnalizeStatement(Statement *statement, size_t index)
{
    if (dynamic_cast<ForLoopStatement *>(statement))
    {
        BlockStatement *blockStatement = gZtoonArena.Allocate<BlockStatement>();
        blockStatement->statements.push_back(statement);
        statements.at(index) = blockStatement;
    }
}

void SemanticAnalyzer::AnalizeStatement(Statement *statement)
{
    if (dynamic_cast<VarDeclStatement *>(statement))
    {
        VarDeclStatement *varDeclStatement =
            dynamic_cast<VarDeclStatement *>(statement);
        stmtToDataTypeMap[varDeclStatement] =
            currentScope->GetDataType(varDeclStatement->GetDataType());

        if (varDeclStatement->GetExpression())
        {
            EvaluateAndAssignDataTypeToExpression(
                varDeclStatement->GetExpression());
            auto arrType = dynamic_cast<ArrayDataType *>(
                stmtToDataTypeMap[varDeclStatement]);
            if (arrType)
            {
                if (arrType && arrType->sizeExpr)
                {
                    auto innerPtrType = arrType;
                    while (innerPtrType)
                    {
                        if (innerPtrType->sizeExpr)
                        {
                            EvaluateAndAssignDataTypeToExpression(
                                innerPtrType->sizeExpr);
                        }
                        innerPtrType = dynamic_cast<ArrayDataType *>(
                            innerPtrType->dataType);
                    }
                }
                ValidateAssignValueToVarArray(varDeclStatement->GetExpression(),
                                              arrType);
            }
            else
            {
                PrimaryExpression *varExpr =
                    gZtoonArena.Allocate<PrimaryExpression>();
                varExpr->primary = varDeclStatement->GetIdentifier();
                varExpr->isLvalue = true;
                Expression *variableRawExpression = varExpr;
                exprToDataTypeMap[varExpr] =
                    stmtToDataTypeMap[varDeclStatement];
                // check if types are compatible.
                DataType::Type dataType = DecideDataType(
                    &(variableRawExpression), &varDeclStatement->expression);
                if (dataType == DataType::Type::UNKNOWN)
                {
                    ReportError(
                        std::format(
                            "Cannot assign value of type '{}' to variable "
                            "of type '{}'",
                            exprToDataTypeMap[varDeclStatement->GetExpression()]
                                ->ToString(),
                            stmtToDataTypeMap[varDeclStatement]->ToString()),
                        varDeclStatement->GetCodeErrString());
                }
            }
        }

        // meaning in global scope.
        if (varDeclStatement->IsGlobal())
        {
            if (!exprToDataTypeMap[varDeclStatement->GetExpression()]
                     ->IsReadOnly() &&
                (!dynamic_cast<PrimaryExpression *>(
                    varDeclStatement->GetExpression())))
            {
                ReportError(
                    std::format("ReadOnly or compile time expression are "
                                "allowd to be "
                                "assigned to global variables"),
                    varDeclStatement->GetExpression()->GetCodeErrString());
            }
            else if (!exprToDataTypeMap[varDeclStatement->GetExpression()]
                          ->IsReadOnly() &&
                     (!dynamic_cast<PrimaryExpression *>(
                         varDeclStatement->GetExpression())) &&
                     dynamic_cast<PrimaryExpression *>(
                         varDeclStatement->GetExpression())
                             ->primary->GetType() == TokenType::IDENTIFIER)
            {

                ReportError(
                    std::format("ReadOnly or compile time expression are "
                                "allowd to be "
                                "assigned to global variables"),
                    varDeclStatement->GetExpression()->GetCodeErrString());
            }
        }

        Variable *var = gZtoonArena.Allocate<Variable>(
            varDeclStatement->GetIdentifier()->GetLexeme());
        var->token = varDeclStatement->GetIdentifier();
        var->dataType = stmtToDataTypeMap[varDeclStatement];
        currentScope->AddSymbol(var, varDeclStatement->GetCodeErrString());
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {
        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);
        EvaluateAndAssignDataTypeToExpression(
            varAssignmentStatement->GetLValue());
        if (varAssignmentStatement->GetLValue()->IsLValue())
        {
            stmtToDataTypeMap[varAssignmentStatement] =
                exprToDataTypeMap[varAssignmentStatement->GetLValue()];
            if (stmtToDataTypeMap[varAssignmentStatement]->IsReadOnly())
            {

                ReportError("Cannot assign value to readonly variable",
                            varAssignmentStatement->GetCodeErrString());
            }
        }
        else
        {
            ReportError("Cannot assign value to r-value expression",
                        varAssignmentStatement->GetCodeErrString());
        }

        EvaluateAndAssignDataTypeToExpression(
            varAssignmentStatement->GetRValue());
        ArrayDataType *arrType = dynamic_cast<ArrayDataType *>(
            exprToDataTypeMap[varAssignmentStatement->GetLValue()]);
        if (arrType)
        {
            ValidateAssignValueToVarArray(varAssignmentStatement->GetRValue(),
                                          arrType);
        }
        else
        {
            DataType::Type dataType =
                DecideDataType(&(varAssignmentStatement->lValue),
                               &varAssignmentStatement->rValue);
            if (dataType == DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Cannot assign value of type '{}' to variable "
                        "of type '{}'",
                        exprToDataTypeMap[varAssignmentStatement->rValue]
                            ->ToString(),
                        stmtToDataTypeMap[varAssignmentStatement]->ToString()),
                    varAssignmentStatement->GetCodeErrString());
            }
        }
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {

        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);

        EvaluateAndAssignDataTypeToExpression(
            varComAssignStatement->GetLValue());
        if (varComAssignStatement->GetLValue()->IsLValue())
        {

            stmtToDataTypeMap[varComAssignStatement] =
                exprToDataTypeMap[varComAssignStatement->GetLValue()];
            if (stmtToDataTypeMap[varComAssignStatement]->IsReadOnly())
            {

                ReportError("Cannot assign value to readonly variable",
                            varComAssignStatement->GetCodeErrString());
            }
        }
        else
        {
            ReportError("Cannot assign value to r-value expression",
                        varComAssignStatement->GetCodeErrString());
        }

        EvaluateAndAssignDataTypeToExpression(
            varComAssignStatement->GetRValue());

        DataType::Type dataType = DecideDataType(
            &(varComAssignStatement->lValue), &varComAssignStatement->rValue);
        if (dataType == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Cannot assign value of type '{}' to variable "
                    "of type '{}'",
                    exprToDataTypeMap[varComAssignStatement->rValue]
                        ->ToString(),
                    stmtToDataTypeMap[varComAssignStatement]->ToString()),
                varComAssignStatement->GetCodeErrString());
        }
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {

        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);

        Scope *scope = gZtoonArena.Allocate<Scope>(this, currentScope);
        Scope *temp = currentScope;

        BlockStatement *blockTemp = currentBlockStatement;
        currentBlockStatement = blockStatement;

        currentScope = scope;
        blockToScopeMap[blockStatement] = scope;

        for (size_t i = 0; i < blockStatement->statements.size(); i++)
        {
            AnalizeStatement(blockStatement->statements[i]);
            blockStatement->index = i;
        }
        currentScope = temp;
        currentBlockStatement = blockTemp;
    }
    else if (dynamic_cast<IfStatement *>(statement))
    {
        IfStatement *ifStatement = dynamic_cast<IfStatement *>(statement);
        EvaluateAndAssignDataTypeToExpression(ifStatement->GetExpression());

        if (exprToDataTypeMap[ifStatement->GetExpression()]->GetType() !=
            DataType::Type::BOOL)
        {
            ReportError(std::format("Expression after 'if' must be boolean."),
                        ifStatement->GetCodeErrString());
        }

        AnalizeStatement(ifStatement->GetBlockStatement());

        for (Statement *s : ifStatement->GetNextElseIforElseStatements())
        {
            AnalizeStatement(s);
        }
    }
    else if (dynamic_cast<ElseIfStatement *>(statement))
    {
        ElseIfStatement *elifStatement =
            dynamic_cast<ElseIfStatement *>(statement);
        EvaluateAndAssignDataTypeToExpression(elifStatement->GetExpression());

        if (exprToDataTypeMap[elifStatement->GetExpression()]->GetType() !=
            DataType::Type::BOOL)
        {
            ReportError(std::format("Expression after 'if' must be boolean."),
                        elifStatement->GetCodeErrString());
        }
        AnalizeStatement(elifStatement->GetBlockStatement());
    }
    else if (dynamic_cast<ElseStatement *>(statement))
    {
        ElseStatement *elseStatement = dynamic_cast<ElseStatement *>(statement);

        AnalizeStatement(elseStatement->GetBlockStatement());
    }
    else if (dynamic_cast<ExpressionStatement *>(statement))
    {
        ExpressionStatement *exprStatement =
            dynamic_cast<ExpressionStatement *>(statement);
        EvaluateAndAssignDataTypeToExpression(exprStatement->GetExpression());
        stmtToDataTypeMap[exprStatement] =
            exprToDataTypeMap[exprStatement->GetExpression()];
    }
    else if (dynamic_cast<WhileLoopStatement *>(statement))
    {
        WhileLoopStatement *whileStatement =
            dynamic_cast<WhileLoopStatement *>(statement);

        EvaluateAndAssignDataTypeToExpression(whileStatement->GetCondition());

        if (exprToDataTypeMap[whileStatement->GetCondition()]->GetType() !=
            DataType::Type::BOOL)
        {
            ReportError(
                std::format(
                    "Expect expression '{}' to be boolean",
                    whileStatement->GetCondition()->GetCodeErrString().str),
                whileStatement->GetCondition()->GetCodeErrString());
        }
        inLoop++;
        AnalizeStatement(whileStatement->GetBlockStatement());
        inLoop--;
    }
    else if (dynamic_cast<ForLoopStatement *>(statement))
    {
        ForLoopStatement *forLoopStatement =
            dynamic_cast<ForLoopStatement *>(statement);

        if (forLoopStatement->GetInit() != nullptr)
            AnalizeStatement(forLoopStatement->GetInit());
        if (forLoopStatement->GetCondition() != nullptr)
        {
            EvaluateAndAssignDataTypeToExpression(
                forLoopStatement->GetCondition());

            if (exprToDataTypeMap[forLoopStatement->GetCondition()]
                    ->GetType() != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Expect expression '{}' to be boolean",
                                forLoopStatement->GetCondition()
                                    ->GetCodeErrString()
                                    .str),
                    forLoopStatement->GetCondition()->GetCodeErrString());
            }
        }
        if (forLoopStatement->GetUpdate())
        {
            AnalizeStatement(forLoopStatement->GetUpdate());
        }
        inLoop++;

        AnalizeStatement(forLoopStatement->GetBlockStatement());
        inLoop--;
    }
    else if (dynamic_cast<BreakStatement *>(statement))
    {
        auto bStmt = dynamic_cast<BreakStatement *>(statement);
        // only in loops and switch stmts
        if (inLoop == 0)
        {
            ReportError("Cannot use 'break' statement outside loop statement",
                        bStmt->GetCodeErrString());
        }
    }
    else if (dynamic_cast<ContinueStatement *>(statement))
    {
        // only in loops
        auto cStmt = dynamic_cast<ContinueStatement *>(statement);
        if (inLoop == 0)
        {
            ReportError(
                "Cannot use 'continue' statement outside loop statement",
                cStmt->GetCodeErrString());
        }
    }
    else if (dynamic_cast<RetStatement *>(statement))
    {
        RetStatement *retStmt = dynamic_cast<RetStatement *>(statement);
        if (retStmt->expression)
        {
            EvaluateAndAssignDataTypeToExpression(retStmt->GetExpression());
            Function *fn = (Function *)currentFunction;
            PrimaryExpression *primaryExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            primaryExpr->primary = gZtoonArena.Allocate<Token>(
                fn->fnPointer->GetReturnDataType()->ToTokenType());
            Expression *expr = primaryExpr;
            exprToDataTypeMap[expr] = fn->fnPointer->GetReturnDataType();
            stmtToDataTypeMap[retStmt] = fn->fnPointer->GetReturnDataType();
            DataType::Type type = DecideDataType(&expr, &retStmt->expression);
            fn->retStmt = retStmt;
            retStmt->fnStmt = fn->GetFnStatement();
            if (type == DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Return expression '{}' is not compatible with "
                        "function return type '{}'",
                        retStmt->GetExpression()->GetCodeErrString().str,
                        currentFunction->fnPointer->returnDataType->ToString()),
                    retStmt->GetCodeErrString());
            }
        }
        else
        {
            DataType::Type type = DataType::Type::NOTYPE;
            if (type !=
                currentFunction->fnPointer->GetReturnDataType()->GetType())
            {
                ReportError(
                    std::format(
                        "Return expression '{}' is not compatible with "
                        "function return type '{}'",
                        retStmt->GetExpression()
                            ? retStmt->GetCodeErrString().str
                            : TokenDataTypeToString(TokenType::NOTYPE),
                        currentFunction->fnPointer->returnDataType->ToString()),
                    retStmt->GetCodeErrString());
            }
            else if (retStmt->expression)
            {

                ReportError(
                    std::format(
                        "Function '{}' does not return a value.",
                        currentFunction->fnPointer->returnDataType->ToString()),
                    retStmt->GetCodeErrString());
            }
        }
    }
    else if (dynamic_cast<FnStatement *>(statement))
    {
        FnStatement *fnStmt = dynamic_cast<FnStatement *>(statement);
        Function *fp = gZtoonArena.Allocate<Function>();
        fp->name = fnStmt->identifier->GetLexeme();
        FnPointerDataType *fpDataType =
            gZtoonArena.Allocate<FnPointerDataType>();
        fpDataType->type = DataType::Type::FNPOINTER;
        fpDataType->returnDataType =
            currentScope->GetDataType(fnStmt->returnDataTypeToken);
        fpDataType->isVarArgs = fnStmt->IsVarArgs();
        fp->fnStmt = fnStmt;
        fp->fnPointer = fpDataType;
        currentScope->AddSymbol(fp, fnStmt->GetCodeErrString());
        Function *temp = currentFunction;
        currentFunction = fp;
        for (Statement *p : fnStmt->parameters)
        {
            AnalizeStatement(p);
            fpDataType->paramters.push_back(stmtToDataTypeMap[p]);
        }
        AnalizeStatement(fnStmt->blockStatement);
        currentFunction = temp;
        // check for ret statement;
        if (!fnStmt->IsPrototype())
        {
            std::function<bool(BlockStatement * bStmt)> checkRet =
                [&](BlockStatement *bStmt) -> bool
            {
                bool retFound = false;
                for (Statement *s : bStmt->statements)
                {
                    BlockStatement *blockStmt =
                        dynamic_cast<BlockStatement *>(s);
                    if (blockStmt)
                    {
                        checkRet(blockStmt);
                    }
                    // if stmt? or loops?
                    IfStatement *ifStmt = dynamic_cast<IfStatement *>(s);
                    RetStatement *retStmt = dynamic_cast<RetStatement *>(s);

                    if (ifStmt)
                    {

                        for (Statement *stmt :
                             ifStmt->GetBlockStatement()->statements)
                        {
                            RetStatement *retStmt =
                                dynamic_cast<RetStatement *>(stmt);
                            if (retStmt)
                            {
                                retFound = true;
                                break;
                            }
                        }

                        if (!retFound)
                        {
                            retFound = checkRet(ifStmt->GetBlockStatement());
                        }
                        if (retFound)
                        {
                            if (ifStmt->GetNextElseIforElseStatements().empty())
                            {
                                retFound = false;
                            }
                            bool foundInOtherPaths = false;
                            for (Statement *stmt :
                                 ifStmt->GetNextElseIforElseStatements())
                            {
                                auto elIfStmt =
                                    dynamic_cast<ElseIfStatement *>(stmt);
                                auto elseStmt =
                                    dynamic_cast<ElseStatement *>(stmt);
                                if (elIfStmt)
                                {
                                    foundInOtherPaths =
                                        checkRet(elIfStmt->GetBlockStatement());
                                }
                                else if (elseStmt)
                                {
                                    foundInOtherPaths =
                                        checkRet(elseStmt->GetBlockStatement());
                                }
                                if (!foundInOtherPaths)
                                {
                                    ReportError("Not all paths return.",
                                                fnStmt->GetCodeErrString());
                                }
                            }

                            retFound = foundInOtherPaths;
                            if (!retFound)
                            {
                                ReportError("Not all paths return.",
                                            fnStmt->GetCodeErrString());
                            }
                        }
                    }
                    else if (retStmt)
                    {
                        return true;
                    }
                }
                return retFound;
            };
            if (fnStmt->returnDataTypeToken->GetDataType()->type !=
                TokenType::NOTYPE)
            {
                if (fnStmt->GetBlockStatement()->statements.empty())
                {
                    ReportError(
                        std::format(
                            "Function '{}' does not return an expression.",
                            fnStmt->GetIdentifier()->GetLexeme()),
                        fnStmt->GetCodeErrString());
                }
                // see if function ends with return statement.
                RetStatement *retStmt = dynamic_cast<RetStatement *>(
                    fnStmt->GetBlockStatement()->statements.back());
                if (!retStmt)
                {
                    bool retFoundInMainBlock = false;
                    for (size_t i;
                         i < fnStmt->GetBlockStatement()->statements.size();
                         i++)
                    {
                        RetStatement *ret = dynamic_cast<RetStatement *>(
                            fnStmt->GetBlockStatement()->statements[i]);
                        if (ret)
                        {
                            retFoundInMainBlock = true;
                        }
                    }
                    if (!retFoundInMainBlock)
                    {

                        if (!checkRet(fnStmt->GetBlockStatement()))
                        {
                            ReportError("Not all paths return.",
                                        fnStmt->GetCodeErrString());
                        }
                    }
                }
            }
        }

        currentScope->datatypesMap[FnPointerDataTypeToStringKey(fpDataType)] =
            fpDataType;
    }
}
void removeReadonlyPrefix(std::string &str)
{
    const std::string prefix = "readonly ";
    const size_t prefixLen = prefix.length();

    // Check if the string starts with "readonly " and erase it
    if (str.compare(0, prefixLen, prefix) == 0)
    {
        str.erase(0, prefixLen);
    }
}
DataType::Type SemanticAnalyzer::DecideDataType(Expression **left,
                                                Expression **right)
{
    DataType *leftDataType = exprToDataTypeMap[*left];
    DataType *rightDataType = exprToDataTypeMap[*right];
    bool isLeftLValue = (*left)->IsLValue();
    bool isRightLValue = (*right)->IsLValue();

    std::string leftDataTypeStr = leftDataType->ToString();
    std::string rightDataTypeStr = rightDataType->ToString();
    removeReadonlyPrefix(leftDataTypeStr);
    removeReadonlyPrefix(rightDataTypeStr);

    if (leftDataTypeStr != rightDataTypeStr)
    {

        if ((*left)->IsLValue() && !(*right)->IsLValue() &&
                (rightDataType->IsInteger()) ||
            rightDataType->IsFloat())
        {
            TokenType leftVarDataType = leftDataType->ToTokenType();
            TokenType rightLiteralDataType = rightDataType->ToTokenType();
            if (IsInteger(leftVarDataType) && IsInteger(rightLiteralDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *right;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(leftDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    leftDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                exprToDataTypeMap[castExpr] = leftDataType;
                *right = castExpr;
                return leftDataType->type;
            }
            else if (IsFloat(leftVarDataType) && IsFloat(rightLiteralDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *right;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(leftDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    leftDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                exprToDataTypeMap[castExpr] = leftDataType;
                *right = castExpr;
                return leftDataType->type;
            }
            else
            {
                // error
                return DataType::Type::UNKNOWN;
            }
        }
        else if ((*right)->IsLValue() && !(*left)->IsLValue() &&
                 (leftDataType->IsInteger() || leftDataType->IsFloat()))
        {

            TokenType leftLiteralDataType = leftDataType->ToTokenType();
            TokenType rightVarDataType = rightDataType->ToTokenType();
            if (IsInteger(leftLiteralDataType) && IsInteger(rightVarDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *left;
                exprToDataTypeMap[castExpr] = rightDataType;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    rightDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                *left = castExpr;
                return rightDataType->type;
            }
            else if (IsFloat(leftLiteralDataType) && IsFloat(rightVarDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *left;
                exprToDataTypeMap[castExpr] = rightDataType;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    rightDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                *left = castExpr;
                return rightDataType->type;
            }
            else
            {
                // error
                return DataType::Type::UNKNOWN;
            }
        }
        else
        {
            return DataType::Type::UNKNOWN;
        }
    }

    return leftDataType->type;
}

void SemanticAnalyzer::EvaluateAndAssignDataTypeToExpression(
    Expression *expression)
{

    if (dynamic_cast<FnCallExpression *>(expression))
    {
        FnCallExpression *fnCallExpr =
            dynamic_cast<FnCallExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(fnCallExpr->GetGetExpression());
        // id or epxr
        FnPointerDataType *fnDataType = dynamic_cast<FnPointerDataType *>(
            exprToDataTypeMap[fnCallExpr->GetGetExpression()]);
        if (!fnDataType)
        {
            ReportError("Expression is not of function pointer type",
                        fnCallExpr->GetCodeErrString());
        }
        exprToDataTypeMap[fnCallExpr] = fnDataType->GetReturnDataType();

        for (Expression *arg : fnCallExpr->args)
        {
            EvaluateAndAssignDataTypeToExpression(arg);
        }
        size_t index = 0;
        for (DataType *argDataType : fnDataType->GetParameters())
        {
            PrimaryExpression *id = gZtoonArena.Allocate<PrimaryExpression>();
            id->primary = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            auto params = fnDataType->GetParameters();
            size_t size = params.size();
            exprToDataTypeMap[id] = params[index];
            Expression *exprId = id;
            if (DecideDataType(&exprId, &fnCallExpr->args[index]) ==
                DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Argument '{}' of type '{}' is not compatible "
                        "with paramter of type '{}'",
                        fnCallExpr->args[index]->GetCodeErrString().str,
                        exprToDataTypeMap[fnCallExpr->args[index]]->ToString(),
                        exprToDataTypeMap[fnCallExpr->args[index]]->ToString()),
                    fnCallExpr->args[index]->GetCodeErrString());
            }
            index++;
        }
        if (fnCallExpr->GetArgs().size() > fnDataType->GetParameters().size())
        {
            if (!fnDataType->IsVarArgs())
            {
                ReportError("function call expression arguments are not "
                            "compatible with function paramters",
                            fnCallExpr->GetCodeErrString());
            }
        }
    }
    else if (dynamic_cast<InitializerListExpression *>(expression))
    {
        auto initListExpr =
            dynamic_cast<InitializerListExpression *>(expression);
        bool allSameType = true;
        DataType *prevType = nullptr;
        InitListType *listType = gZtoonArena.Allocate<InitListType>();
        listType->isReadOnly = true;
        listType->type = DataType::Type::InitList;

        if (initListExpr->GetExpressions().empty())
        {
            ReportError("Initializer list cannot be empty",
                        initListExpr->GetCodeErrString());
        }

        for (auto expr : initListExpr->expressions)
        {
            EvaluateAndAssignDataTypeToExpression(expr);
            auto type = exprToDataTypeMap[expr];
            listType->dataTypes.push_back(type);
            if (prevType && allSameType)
            {
                if (prevType->type == type->type)
                {
                    allSameType = true;
                }
                else
                {
                    allSameType = false;
                }
            }
            prevType = type;
        }
        listType->allSameType = allSameType;
        exprToDataTypeMap[initListExpr] = listType;
    }
    else if (dynamic_cast<TernaryExpression *>(expression))
    {
        TernaryExpression *ternaryExpr =
            dynamic_cast<TernaryExpression *>(expression);

        EvaluateAndAssignDataTypeToExpression(ternaryExpr->condition);
        EvaluateAndAssignDataTypeToExpression(ternaryExpr->trueExpr);
        EvaluateAndAssignDataTypeToExpression(ternaryExpr->falseExpr);

        if (exprToDataTypeMap[ternaryExpr->condition]->GetType() !=
            DataType::Type::BOOL)
        {
            ReportError("Expected expression to be boolean type",
                        ternaryExpr->GetCodeErrString());
        }

        DataType::Type type =
            DecideDataType(&ternaryExpr->trueExpr, &ternaryExpr->falseExpr);

        if (type == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Expression '{}' and '{}' must be of the same type.",
                    ternaryExpr->trueExpr->GetCodeErrString().str,
                    ternaryExpr->falseExpr->GetCodeErrString().str),
                ternaryExpr->GetCodeErrString());
        }

        exprToDataTypeMap[ternaryExpr] =
            exprToDataTypeMap[ternaryExpr->trueExpr];
    }
    else if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(
            binaryExpression->GetLeftExpression());
        EvaluateAndAssignDataTypeToExpression(
            binaryExpression->GetRightExpression());

        DataType *left = exprToDataTypeMap[binaryExpression->left];
        DataType *right = exprToDataTypeMap[binaryExpression->right];

        DataType::Type dataType =
            DecideDataType(&binaryExpression->left, &binaryExpression->right);

        exprToDataTypeMap[binaryExpression] = left;
        if (dataType == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format("Cannot perform '{}' on datatypes '{}' and '{}'",
                            binaryExpression->op->GetLexeme(), left->ToString(),
                            right->ToString()),
                binaryExpression->GetCodeErrString());
            return;
        }

        switch (binaryExpression->op->GetType())
        {
        case TokenType::PLUS:
        case TokenType::DASH:
        case TokenType::ASTERISK:
        case TokenType::SLASH:
        case TokenType::PERCENTAGE:
        {
            if (!left->IsNumerical())
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!right->IsNumerical())
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::BITWISE_AND:
        case TokenType::BITWISE_OR:
        case TokenType::BITWISE_XOR:
        case TokenType::SHIFT_LEFT:
        case TokenType::SHIFT_RIGHT:
        {
            if (!left->IsInteger())
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "integer type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!right->IsInteger())
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "integer type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::EQUAL_EQUAL:
        case TokenType::EXCLAMATION_EQUAL:
        case TokenType::LESS:
        case TokenType::LESS_EQUAL:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUAL:
        {

            if (!left->IsNumerical() && left->type != DataType::Type::BOOL)
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "boolean or numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!right->IsNumerical() &&
                     right->type != DataType::Type::BOOL)
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "boolean or numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            exprToDataTypeMap[binaryExpression] =
                currentScope->datatypesMap["bool"];
            break;
        }
        case TokenType::OR:
        case TokenType::AND:
        {
            if (left->type != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Left expression of '{}' must be boolean type",
                                binaryExpression->op->GetLexeme()),
                    binaryExpression->GetCodeErrString());
            }
            else if (right->type != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Right expression of '{}' must be boolean type",
                                binaryExpression->op->GetLexeme()),
                    binaryExpression->GetCodeErrString());
            }
            exprToDataTypeMap[binaryExpression] =
                currentScope->datatypesMap["bool"];
            break;
        }
        default:
        {
            break;
        }
        }
    }
    else if (dynamic_cast<UnaryExpression *>(expression))
    {
        UnaryExpression *unaryExpression =
            dynamic_cast<UnaryExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(
            unaryExpression->GetRightExpression());
        DataType *rightDataType = exprToDataTypeMap[unaryExpression->right];
        exprToDataTypeMap[unaryExpression] = rightDataType;
        switch (unaryExpression->GetOperator()->GetType())
        {
        case TokenType::DASH:
        {
            // numerical
            if (!rightDataType->IsNumerical() && !rightDataType->IsSigned())
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::TILDE:
        {
            if (!rightDataType->IsInteger())
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::DASH_DASH:
        case TokenType::PLUS_PLUS:
        {
            // numerical
            if (!unaryExpression->GetRightExpression()->IsLValue())
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on r-value "
                                "expression",
                                unaryExpression->GetOperator()->GetLexeme()),
                    unaryExpression->GetRightExpression()->GetCodeErrString());
            }

            if (!rightDataType->IsNumerical())
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            if (unaryExpression->IsPostfix())
            {
                // Insert new variable assign statement with binary
                VarAssignmentStatement *varAssignStatement =
                    gZtoonArena.Allocate<VarAssignmentStatement>();
                Token *typeToken =
                    gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
                varAssignStatement->lValue =
                    unaryExpression->GetRightExpression();

                BinaryExpression *binaryExpr =
                    gZtoonArena.Allocate<BinaryExpression>();
                binaryExpr->left = unaryExpression->GetRightExpression();
                PrimaryExpression *oneExpr =
                    gZtoonArena.Allocate<PrimaryExpression>();
                exprToDataTypeMap[oneExpr] = rightDataType;

                Token *oneToken = nullptr;
                if (rightDataType->IsInteger())
                {
                    oneToken = gZtoonArena.Allocate<TokenLiteral<int32_t>>(
                        TokenType::INTEGER_LITERAL, 1);
                }
                else if (rightDataType->IsFloat())
                {
                    oneToken = gZtoonArena.Allocate<TokenLiteral<float>>(
                        TokenType::FLOAT_LITERAL, 1.0);
                }
                oneExpr->primary = oneToken;
                binaryExpr->right = oneExpr;
                exprToDataTypeMap[binaryExpr] = rightDataType;
                binaryExpr->op =
                    unaryExpression->GetOperator()->type == TokenType::PLUS_PLUS
                        ? gZtoonArena.Allocate<Token>(TokenType::PLUS)
                        : gZtoonArena.Allocate<Token>(TokenType::DASH);
                varAssignStatement->rValue = binaryExpr;
                // operation directly after this statement.
                // need to know if inside block statement or no.
                // if inside, need to get block statement.

                if (currentBlockStatement)
                {
                    // inside
                    size_t s = currentBlockStatement->statements.size();
                    currentBlockStatement->statements.insert(
                        currentBlockStatement->statements.begin() +
                            currentBlockStatement->index + 1,
                        varAssignStatement);
                }
                // disable this expression.
                unaryExpression->op =
                    gZtoonArena.Allocate<Token>(TokenType::PLUS);
            }

            break;
        }
        case TokenType::PLUS:
        {
            // numerical
            if (!rightDataType->IsNumerical())
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::EXCLAMATION:
        {
            // numerical
            if (rightDataType->type != DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::SIZEOF:
        {
            if (rightDataType->type == DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                rightDataType->ToString()),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }

        case TokenType::ASTERISK:
        {
            // deref works on ptrs.
            if (rightDataType->type != DataType::Type::POINTER)
            {
                ReportError(std::format("Cannot derefrence non-pointer types"),
                            unaryExpression->GetCodeErrString());
            }
            PointerDataType *ptrDataType =
                dynamic_cast<PointerDataType *>(rightDataType);
            unaryExpression->isLvalue = true;
            exprToDataTypeMap[unaryExpression] =
                ptrDataType->PointedToDatatype();
            break;
        }
        case TokenType::BITWISE_AND:
        {
            // ref only works on lvalue
            if (!unaryExpression->GetRightExpression()->IsLValue())
            {
                ReportError("Cannot reference r-value expressions",
                            unaryExpression->GetCodeErrString());
            }

            PointerDataType *ptrDataType =
                gZtoonArena.Allocate<PointerDataType>();
            ptrDataType->type = DataType::Type::POINTER;
            ptrDataType->dataType = rightDataType;
            currentScope->datatypesMap[ptrDataType->ToString()] = ptrDataType;
            ptrDataType->isReadOnly = true;
            exprToDataTypeMap[unaryExpression] = ptrDataType;
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
        EvaluateAndAssignDataTypeToExpression(
            groupingExpression->GetExpression());
        exprToDataTypeMap[groupingExpression] =
            exprToDataTypeMap[groupingExpression->GetExpression()];
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(castExpression->GetExpression());

        exprToDataTypeMap[castExpression] =
            currentScope->GetDataType(castExpression->castToTypeToken);

        DataType *valueType = exprToDataTypeMap[castExpression->expression];
        DataType *castType = exprToDataTypeMap[castExpression];
        if (castType->GetType() == DataType::Type::BOOL)
        {
            if (exprToDataTypeMap[castExpression]->GetType() !=
                DataType::Type::BOOL)
            {
                ReportError(
                    std::format(
                        "Cannot Cast from '{}' datatype to '{}' datatype",
                        valueType->ToString(), castType->ToString()),
                    castExpression->GetCodeErrString());
            }
        }
        else if (castType->GetType() == DataType::Type::ARRAY)
        {
            ReportError(std::format("Cannot cast to array type"),
                        castExpression->GetCodeErrString());
        }
        else if ((valueType->GetType() == DataType::Type::ARRAY) &&
                 castType->GetType() == DataType::Type::POINTER)
        {
            // check if the pointer is a pointer to the inner type of the array.
            auto arrType = dynamic_cast<ArrayDataType *>(valueType);
            auto ptrType = dynamic_cast<PointerDataType *>(castType);

            if (arrType->dataType->ToString() != ptrType->dataType->ToString())
            {
                ReportError(std::format("Cannot cast from '{}' to '{}'",
                                        arrType->ToString(),
                                        ptrType->ToString()),
                            castExpression->GetCodeErrString());
            }
        }
        // ptr stuff
        //  ptr to ptr ok
        //  int to ptr ok
        //  ptr to int ok
        //  else not ok.
        else if (castType->GetType() == DataType::Type::POINTER &&
                 (valueType->GetType() != DataType::Type::POINTER &&
                  !valueType->IsInteger()))
        {
            ReportError(
                std::format("Cannot Cast from '{}' datatype to '{}' datatype",
                            valueType->ToString(), castType->ToString()),
                castExpression->GetCodeErrString());
        }

        else if ((valueType->GetType() == DataType::Type::POINTER) &&
                 !castType->IsInteger())
        {
            ReportError(std::format("Pointer datatype can only be casted to an "
                                    "integer datatype"),
                        castExpression->GetCodeErrString());
        }
    }
    else if (dynamic_cast<SubscriptExpression *>(expression))
    {
        SubscriptExpression *subExpr =
            dynamic_cast<SubscriptExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(subExpr->GetExpression());

        DataType *exprDataType = exprToDataTypeMap[subExpr->GetExpression()];
        auto ptrType = dynamic_cast<PointerDataType *>(exprDataType);
        auto arrType = dynamic_cast<ArrayDataType *>(exprDataType);
        if (!ptrType && !arrType)
        {
            ReportError(
                "Subscript operator only works on pointer and array type",
                subExpr->GetExpression()->GetCodeErrString());
        }
        EvaluateAndAssignDataTypeToExpression(subExpr->GetIndexExpression());
        DataType *indexDataType =
            exprToDataTypeMap[subExpr->GetIndexExpression()];
        if (!indexDataType->IsInteger())
        {
            ReportError("Index expression must be integer type",
                        subExpr->GetIndexExpression()->GetCodeErrString());
        }
        if (ptrType)
        {
            exprToDataTypeMap[subExpr] = ptrType->PointedToDatatype();
        }
        else if (arrType)
        {
            exprToDataTypeMap[subExpr] = arrType->dataType;
        }
    }
    else if (dynamic_cast<PrimaryExpression *>(expression))
    {
        PrimaryExpression *primaryExpression =
            dynamic_cast<PrimaryExpression *>(expression);
        switch (primaryExpression->GetPrimary()->GetType())
        {
        case TokenType::INTEGER_LITERAL:
        {
            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["i32"];
            break;
        }
        case TokenType::FLOAT_LITERAL:
        {
            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["f32"];
            break;
        }
        case TokenType::STRING_LITERAL:
        {
            auto strLiteral = dynamic_cast<TokenLiteral<std::string> const *>(
                primaryExpression->primary);

            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["readonly i8*"];
            break;
        }
        case TokenType::CHARACTER_LITERAL:
        {
            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["i8"];
            break;
        }
        case TokenType::TRUE:
        case TokenType::FALSE:
        {
            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["bool"];
            break;
        }
        case TokenType::IDENTIFIER:
        {
            primaryExpression->isLvalue = true;
            exprToDataTypeMap[primaryExpression] =
                currentScope
                    ->GetSymbol(primaryExpression->primary->GetLexeme(),
                                primaryExpression->GetCodeErrString())
                    ->GetDataType();
            break;
        }
        default:
        {
            break;
        }
        }
    }
    else
    {
        ReportError("This expression is not supported.",
                    expression->GetCodeErrString());
    };
}
