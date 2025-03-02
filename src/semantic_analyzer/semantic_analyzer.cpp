#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include "llvm/IR/Metadata.h"
#include <format>
#include <functional>
#include <utility>

std::string DataType::ToString()
{
    switch (type)
    {
    case DataType::Type::I8:
        return "i8";
    case DataType::Type::I16:
        return "i16";
    case DataType::Type::I32:
        return "i32";
    case DataType::Type::I64:
        return "i64";
    case DataType::Type::U8:
        return "u8";
    case DataType::Type::U16:
        return "u16";
    case DataType::Type::U32:
        return "u32";
    case DataType::Type::U64:
        return "u64";
    case DataType::Type::F32:
        return "f32";
    case DataType::Type::F64:
        return "f64";
    case DataType::Type::BOOL:
        return "bool";
    case DataType::Type::NOTYPE:
        return "notype";
    case DataType::Type::STRING:
        return "string";
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
        return AggregateTypeToString();
    default:
        return "Unknown type";
    }
}

std::string FnPointerDataTypeToStringKey(FnPointerDataType *fnDataType)
{

    std::string fpDatatypeStringKey = "fn ";
    for (DataType *paramDatatype : fnDataType->GetParameters())
    {
        fpDatatypeStringKey += std::format("{} ", paramDatatype->ToString());
    }
    fpDatatypeStringKey += fnDataType->GetReturnDataType()->ToString();
    return fpDatatypeStringKey;
}
Scope::Scope(Scope *parent)
{
    this->parent = parent;

    datatypesMap["i8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i8"]->type = DataType::Type::I8;
    datatypesMap["i8"]->typeWidth = 8;
    datatypesMap["i8"]->alignment = 4;

    datatypesMap["i16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i16"]->type = DataType::Type::I16;
    datatypesMap["i16"]->typeWidth = 16;
    datatypesMap["i16"]->alignment = 4;

    datatypesMap["i32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i32"]->type = DataType::Type::I32;
    datatypesMap["i32"]->typeWidth = 32;
    datatypesMap["i32"]->alignment = 4;

    datatypesMap["i64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i64"]->type = DataType::Type::I64;
    datatypesMap["i64"]->typeWidth = 64;
    datatypesMap["i64"]->alignment = 4;

    datatypesMap["u8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u8"]->type = DataType::Type::U8;
    datatypesMap["u8"]->typeWidth = 8;
    datatypesMap["u8"]->alignment = 4;

    datatypesMap["u16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u16"]->type = DataType::Type::U16;
    datatypesMap["u16"]->typeWidth = 16;
    datatypesMap["u16"]->alignment = 4;

    datatypesMap["u32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u32"]->type = DataType::Type::U32;
    datatypesMap["u32"]->typeWidth = 32;
    datatypesMap["u32"]->alignment = 4;

    datatypesMap["u64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u64"]->type = DataType::Type::U64;
    datatypesMap["u64"]->typeWidth = 64;
    datatypesMap["u64"]->alignment = 4;

    datatypesMap["f32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f32"]->type = DataType::Type::F32;
    datatypesMap["f32"]->typeWidth = 32;
    datatypesMap["f32"]->alignment = 4;

    datatypesMap["f64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f64"]->type = DataType::Type::F64;
    datatypesMap["f64"]->typeWidth = 64;
    datatypesMap["f64"]->alignment = 4;

    datatypesMap["bool"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["bool"]->type = DataType::Type::BOOL;
    datatypesMap["bool"]->typeWidth = 1;
    datatypesMap["bool"]->alignment = 4;

    datatypesMap["notype"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["notype"]->type = DataType::Type::NOTYPE;
    datatypesMap["notype"]->typeWidth = 1;
    datatypesMap["notype"]->alignment = 4;
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
    case DataType::Type::STRING:
        return TokenType::STR;
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
        return TokenType::IDENTIFIER;
    default:
        return TokenType::UNKNOWN;
    }
}

Variable *Scope::GetVariable(std::string name, CodeErrString codeErrString)
{
    Variable *var = nullptr;

    Scope const *current = this;
    while (!var)
    {
        if (current->variablesMap.contains(name))
        {
            var = current->variablesMap.at(name);
        }
        // go up one level;
        if (!current->parent)
        {
            // no more scopes.
            break;
        }
        current = current->parent;
    }

    if (var)
    {
        return var;
    }
    else
    {
        ReportError(
            std::format("Variable '{}' is not defined in this scope.", name),
            codeErrString);
    }
    return nullptr;
}

void Scope::AddVariable(Variable *variable, CodeErrString codeErrString)
{
    if (variablesMap.contains(variable->GetName()))
    {
        Variable *alreadyDefinedVar = variablesMap[variable->GetName()];
        ReportError(std::format("Variable '{}' already defined at {}:{}.",
                                variable->GetName(),
                                alreadyDefinedVar->GetToken()->GetFilename(),
                                alreadyDefinedVar->GetToken()->GetLineNumber()),
                    codeErrString);
    }
    else
    {
        variablesMap[variable->GetName()] = variable;
    }
}

void Scope::AddFunction(Function *function, CodeErrString codeErrString)
{

    if (functionsMap.contains(function->GetName()))
    {
        Function *alreadyDefinedVar = functionsMap[function->GetName()];
        ReportError(std::format("function '{}' already defined at {}:{}.",
                                function->GetName(),
                                alreadyDefinedVar->GetFnStatement()
                                    ->GetIdentifier()
                                    ->GetFilename(),
                                alreadyDefinedVar->GetFnStatement()
                                    ->GetIdentifier()
                                    ->GetLineNumber()),
                    codeErrString);
    }
    else
    {
        functionsMap[function->GetName()] = function;
    }
}
Function *Scope::GetFunction(std::string name, CodeErrString codeErrString)
{

    Function *fn = nullptr;

    Scope const *current = this;
    while (!fn)
    {
        if (current->functionsMap.contains(name))
        {
            fn = current->functionsMap.at(name);
        }
        // go up one level;
        if (!current->parent)
        {
            // no more scopes.
            break;
        }
        current = current->parent;
    }

    if (fn)
    {
        return fn;
    }
    else
    {
        ReportError(
            std::format("Function '{}' is not declared in this scope.", name),
            codeErrString);
    }
    return nullptr;
}
SemanticAnalyzer::SemanticAnalyzer(std::vector<Statement *> &statements)
    : statements(statements)
{
    currentScope = gZtoonArena.Allocate<Scope>();
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
        if (varDeclStatement->GetExpression())
        {
            EvaluateAndAssignDataTypeToExpression(
                varDeclStatement->GetExpression());
            stmtToDataTypeMap[varDeclStatement] =
                exprToDataTypeMap[varDeclStatement->expression];

            PrimaryExpression *varExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            varExpr->primary = varDeclStatement->GetIdentifier();
            Expression *variableRawExpression = varExpr;
            exprToDataTypeMap[varExpr] = stmtToDataTypeMap[varDeclStatement];
            // check if types are compatible.
            DataType::Type dataType = DecideDataType(
                &(variableRawExpression), &varDeclStatement->expression);
            if (dataType == DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Cannot assign value of type '{}' to variable "
                        "of type '{}'",
                        stmtToDataTypeMap[varDeclStatement]->ToString(),
                        stmtToDataTypeMap[varDeclStatement]->ToString()),
                    varDeclStatement->GetCodeErrString());
            }
        }
        Variable *var = gZtoonArena.Allocate<Variable>(
            varDeclStatement->GetIdentifier()->GetLexeme());
        var->token = varDeclStatement->GetIdentifier();
        var->dataType =
            currentScope
                ->datatypesMap[stmtToDataTypeMap[varDeclStatement]->ToString()];
        currentScope->AddVariable(var, varDeclStatement->GetCodeErrString());
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {

        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);

        Variable const *var = currentScope->GetVariable(
            varAssignmentStatement->GetIdentifier()->GetLexeme(),
            varAssignmentStatement->GetCodeErrString());

        stmtToDataTypeMap[varAssignmentStatement] = var->dataType;
        EvaluateAndAssignDataTypeToExpression(
            varAssignmentStatement->GetExpression());

        PrimaryExpression *varExpr = gZtoonArena.Allocate<PrimaryExpression>();
        varExpr->primary = varAssignmentStatement->GetIdentifier();
        Expression *variableRawExpression = varExpr;
        exprToDataTypeMap[varExpr] =
            stmtToDataTypeMap[varAssignmentStatement]; // check if types are
                                                       // compatible.
        DataType::Type dataType = DecideDataType(
            &(variableRawExpression), &varAssignmentStatement->expression);
        if (dataType == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Cannot assign value of type '{}' to variable "
                    "of type '{}'",
                    stmtToDataTypeMap[varAssignmentStatement]->ToString(),
                    stmtToDataTypeMap[varAssignmentStatement]->ToString()),
                varAssignmentStatement->GetCodeErrString());
        }
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {

        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);

        Variable const *var = currentScope->GetVariable(
            varComAssignStatement->GetIdentifier()->GetLexeme(),
            varComAssignStatement->GetCodeErrString());
        stmtToDataTypeMap[varComAssignStatement] = var->dataType;

        EvaluateAndAssignDataTypeToExpression(
            varComAssignStatement->GetExpression());
        PrimaryExpression *varExpr = gZtoonArena.Allocate<PrimaryExpression>();
        varExpr->primary = varComAssignStatement->GetIdentifier();
        Expression *variableRawExpression = varExpr;

        exprToDataTypeMap[varExpr] =
            stmtToDataTypeMap[varComAssignStatement]; // check if types are
                                                      // compatible.
        DataType::Type dataType = DecideDataType(
            &(variableRawExpression), &varComAssignStatement->expression);
        if (dataType == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Cannot assign value of type '{}' to variable "
                    "of type '{}'",
                    stmtToDataTypeMap[varComAssignStatement]->ToString(),
                    stmtToDataTypeMap[varComAssignStatement]->ToString()),
                varComAssignStatement->GetCodeErrString());
        }
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {

        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);

        Scope *scope = gZtoonArena.Allocate<Scope>(currentScope);
        Scope *temp = currentScope;

        BlockStatement *blockTemp = currentScope->currentBlockStatement;
        currentScope->currentBlockStatement = blockStatement;

        currentScope = scope;
        blockToScopeMap[blockStatement] = scope;

        for (size_t i = 0; i < blockStatement->statements.size(); i++)
        {
            AnalizeStatement(blockStatement->statements[i]);
            blockStatement->index = i;
        }
        currentScope = temp;
        currentScope->currentBlockStatement = blockTemp;
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

        AnalizeStatement(whileStatement->GetBlockStatement());
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

        AnalizeStatement(forLoopStatement->GetBlockStatement());
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
        if (currentScope->parent)
        {
            ReportError("Functions can only be declared in global scope.",
                        statement->GetCodeErrString());
        }
        FnStatement *fnStmt = dynamic_cast<FnStatement *>(statement);

        Function *fp = gZtoonArena.Allocate<Function>();
        fp->name = fnStmt->identifier->GetLexeme();
        FnPointerDataType *fpDataType =
            gZtoonArena.Allocate<FnPointerDataType>();
        fpDataType->type = DataType::Type::FNPOINTER;
        fpDataType->returnDataType =
            currentScope->datatypesMap[fnStmt->returnDataType->GetLexeme()];

        fp->fnStmt = fnStmt;
        fp->fnPointer = fpDataType;
        currentScope->AddFunction(fp, fnStmt->GetCodeErrString());
        // this should be handled by the
        // function block.
        //  for (Statement *p : fnStmt->parameters)
        //  {
        //      AnalizeStatement(p);
        //  }
        Function *temp = currentFunction;
        currentFunction = fp;
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
            if (fnStmt->returnDataType->type != TokenType::NOTYPE)
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

        for (VarDeclStatement *p : fnStmt->GetParameters())
        {
            fpDataType->paramters.push_back(stmtToDataTypeMap[p]);
        }

        currentScope->datatypesMap[FnPointerDataTypeToStringKey(fpDataType)] =
            fpDataType;
    }
}

DataType::Type SemanticAnalyzer::DecideDataType(Expression **left,
                                                Expression **right)
{
    DataType *leftDataType = exprToDataTypeMap[*left];
    DataType *rightDataType = exprToDataTypeMap[*right];
    bool isLeftPrimary = dynamic_cast<PrimaryExpression *>((*left));
    bool isRightPrimary = dynamic_cast<PrimaryExpression *>((*right));
    if (leftDataType->ToString() != rightDataType->ToString())
    {
        if (isLeftPrimary && isRightPrimary)
        {
            PrimaryExpression *leftPrimaryExpr =
                dynamic_cast<PrimaryExpression *>((*left));

            PrimaryExpression *rightPrimaryExpr =
                dynamic_cast<PrimaryExpression *>((*right));

            if (leftPrimaryExpr->GetPrimary()->GetType() ==
                        TokenType::IDENTIFIER &&
                    rightPrimaryExpr->GetPrimary()->GetType() !=
                        TokenType::IDENTIFIER &&
                    (rightDataType->IsInteger()) ||
                rightDataType->IsFloat())
            {
                TokenType leftVarDataType = leftDataType->ToTokenType();
                TokenType rightLiteralDataType = rightDataType->ToTokenType();
                if (IsInteger(leftVarDataType) &&
                    IsInteger(rightLiteralDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *right;
                    castExpr->castToType = gZtoonArena.Allocate<Token>(
                        leftDataType->ToTokenType());
                    exprToDataTypeMap[castExpr] = leftDataType;
                    *right = castExpr;
                    return leftDataType->type;
                }
                else if (IsFloat(leftVarDataType) &&
                         IsFloat(rightLiteralDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *right;
                    castExpr->castToType = gZtoonArena.Allocate<Token>(
                        leftDataType->ToTokenType());
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
            else if (rightPrimaryExpr->GetPrimary()->GetType() ==
                         TokenType::IDENTIFIER &&
                     leftPrimaryExpr->GetPrimary()->GetType() !=
                         TokenType::IDENTIFIER &&
                     (leftDataType->IsInteger() || leftDataType->IsFloat()))
            {

                TokenType leftLiteralDataType = leftDataType->ToTokenType();
                TokenType rightVarDataType = rightDataType->ToTokenType();
                if (IsInteger(leftLiteralDataType) &&
                    IsInteger(rightVarDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *left;
                    exprToDataTypeMap[castExpr] = rightDataType;
                    castExpr->castToType = gZtoonArena.Allocate<Token>(
                        rightDataType->ToTokenType());
                    *left = castExpr;
                    return rightDataType->type;
                }
                else if (IsFloat(leftLiteralDataType) &&
                         IsFloat(rightVarDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *left;
                    exprToDataTypeMap[castExpr] = leftDataType;
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
        Function *fp = (Function *)currentScope->GetFunction(
            fnCallExpr->identifier->GetLexeme(),
            fnCallExpr->GetCodeErrString());
        exprToDataTypeMap[fnCallExpr] = fp->fnPointer->returnDataType;
        for (Expression *arg : fnCallExpr->args)
        {
            EvaluateAndAssignDataTypeToExpression(arg);
        }
        size_t index = 0;
        for (VarDeclStatement *p : fp->GetFnStatement()->parameters)
        {
            PrimaryExpression *id = gZtoonArena.Allocate<PrimaryExpression>();
            id->primary = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            exprToDataTypeMap[id] = stmtToDataTypeMap[p];
            Expression *exprId = id;
            if (DecideDataType(&exprId, &fnCallExpr->args[index]) ==
                DataType::Type::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Argument '{}' of type '{}' is not compatible "
                        "with paramter '{}' of type '{}'",
                        fnCallExpr->args[index]->GetCodeErrString().str,

                        exprToDataTypeMap[fnCallExpr->args[index]]->ToString(),
                        p->GetCodeErrString().str,
                        stmtToDataTypeMap[p]->ToString()),
                    fnCallExpr->args[index]->GetCodeErrString());
            }
            index++;
        }
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
            PrimaryExpression *primaryExpr = dynamic_cast<PrimaryExpression *>(
                unaryExpression->GetRightExpression());
            if (primaryExpr)
            {
                if (!currentScope->GetVariable(
                        primaryExpr->primary->GetLexeme(),
                        primaryExpr->GetCodeErrString()))
                {
                    ReportError(
                        std::format(
                            "Cannot perform unary operator '{}' on "
                            "non l-value expression",
                            unaryExpression->GetOperator()->GetLexeme()),
                        unaryExpression->GetCodeErrString());
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
                    Token *typeToken = gZtoonArena.Allocate<Token>(
                        rightDataType->ToTokenType());
                    varAssignStatement->dataType = typeToken;
                    varAssignStatement->identifier = primaryExpr->GetPrimary();

                    BinaryExpression *binaryExpr =
                        gZtoonArena.Allocate<BinaryExpression>();
                    binaryExpr->left = primaryExpr;
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
                        unaryExpression->GetOperator()->type ==
                                TokenType::PLUS_PLUS
                            ? gZtoonArena.Allocate<Token>(TokenType::PLUS)
                            : gZtoonArena.Allocate<Token>(TokenType::DASH);
                    varAssignStatement->expression = binaryExpr;
                    // operation directly after this statement.
                    // need to know if inside block statement or no.
                    // if inside, need to get block statement.

                    if (currentScope->currentBlockStatement)
                    {
                        // inside
                        size_t s = currentScope->currentBlockStatement
                                       ->statements.size();
                        currentScope->currentBlockStatement->statements.insert(
                            currentScope->currentBlockStatement->statements
                                    .begin() +
                                currentScope->currentBlockStatement->index + 1,
                            varAssignStatement);
                    }
                    else
                    {
                        // global
                        statements.insert(statements.begin() +
                                              statementCurrentIndex + 1,
                                          varAssignStatement);
                    }

                    // disable this expression.
                    unaryExpression->op =
                        gZtoonArena.Allocate<Token>(TokenType::PLUS);
                }
            }
            else
            {
                ReportError(
                    std::format("Expression '{}' must be l-value",
                                unaryExpression->right->GetCodeErrString().str),
                    unaryExpression->right->GetCodeErrString());
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
            if (rightDataType->type == DataType::Type::BOOL)
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
            currentScope->datatypesMap[castExpression->castToType->GetLexeme()];
        if (exprToDataTypeMap[castExpression]->GetType() ==
            DataType::Type::BOOL)
        {
            if (exprToDataTypeMap[castExpression]->GetType() !=
                DataType::Type::BOOL)
            {
                ReportError(
                    std::format("Cannot cast datatype '{}' to datatype '{}'",
                                exprToDataTypeMap[castExpression->expression]
                                    ->ToString(),
                                exprToDataTypeMap[castExpression]->ToString()),
                    castExpression->GetCodeErrString());
            }
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
            StringDataType *strType = gZtoonArena.Allocate<StringDataType>();
            auto strLiteral = dynamic_cast<TokenLiteral<std::string> const *>(
                primaryExpression->primary);
            strType->len = strLiteral->value.length();
            strType->sizeInBytes = strLiteral->value.size();
            strType->dataType = currentScope->datatypesMap["u8"];
            exprToDataTypeMap[primaryExpression] = strType;
            break;
        }
        case TokenType::CHARACTER_LITERAL:
        {
            exprToDataTypeMap[primaryExpression] =
                currentScope->datatypesMap["u8"];
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
            exprToDataTypeMap[primaryExpression] =
                currentScope
                    ->GetVariable(primaryExpression->primary->GetLexeme(),
                                  primaryExpression->GetCodeErrString())
                    ->dataType;
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
