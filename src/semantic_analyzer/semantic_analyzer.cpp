#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include "llvm/IR/Analysis.h"
#include <format>
#include <iterator>
Variable const *Scope::GetVariable(std::string name,
                                   CodeErrString codeErrString) const
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
        ReportError(
            std::format("Variable '{}' already defined at {}:{}.",
                        variable->GetName(),
                        alreadyDefinedVar->GetIdToken()->GetFilename(),
                        alreadyDefinedVar->GetIdToken()->GetLineNumber()),
            codeErrString);
    }
    else
    {
        variablesMap[variable->GetName()] = variable;
    }
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

            PrimaryExpression *varExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            varExpr->primary = varDeclStatement->GetIdentifier();
            varExpr->dataType = varDeclStatement->GetDataType()->GetType();
            Expression *variableRawExpression = varExpr;
            // check if types are compatible.
            TokenType dataType = DecideDataType(&(variableRawExpression),
                                                &varDeclStatement->expression);
            if (dataType == TokenType::UNKNOWN)
            {
                ReportError(
                    std::format("Cannot assign value of type '{}' to variable "
                                "of type '{}'",
                                TokenDataTypeToString(
                                    varDeclStatement->expression->dataType),
                                varDeclStatement->GetDataType()->GetLexeme()),
                    varDeclStatement->GetCodeErrString());
            }
        }
        Variable *var = gZtoonArena.Allocate<Variable>(
            varDeclStatement->GetIdentifier()->GetLexeme(),
            varDeclStatement->GetDataType(), varDeclStatement->GetIdentifier());
        currentScope->AddVariable(var, varDeclStatement->GetCodeErrString());
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {

        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);

        Variable const *var = currentScope->GetVariable(
            varAssignmentStatement->GetIdentifier()->GetLexeme(),
            varAssignmentStatement->GetCodeErrString());

        varAssignmentStatement->dataType = var->GetDataType();
        EvaluateAndAssignDataTypeToExpression(
            varAssignmentStatement->GetExpression());

        PrimaryExpression *varExpr = gZtoonArena.Allocate<PrimaryExpression>();
        varExpr->primary = varAssignmentStatement->GetIdentifier();
        varExpr->dataType = varAssignmentStatement->GetDataType()->GetType();
        Expression *variableRawExpression = varExpr;
        // check if types are compatible.
        TokenType dataType = DecideDataType(
            &(variableRawExpression), &varAssignmentStatement->expression);
        if (dataType == TokenType::UNKNOWN)
        {
            ReportError(
                std::format("Cannot assign value of type '{}' to variable "
                            "of type '{}'",
                            TokenDataTypeToString(
                                varAssignmentStatement->expression->dataType),
                            varAssignmentStatement->GetDataType()->GetLexeme()),
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
        varComAssignStatement->dataType = var->GetDataType();

        EvaluateAndAssignDataTypeToExpression(
            varComAssignStatement->GetExpression());
        PrimaryExpression *varExpr = gZtoonArena.Allocate<PrimaryExpression>();
        varExpr->primary = varComAssignStatement->GetIdentifier();
        varExpr->dataType = varComAssignStatement->GetDataType()->GetType();
        Expression *variableRawExpression = varExpr;
        // check if types are compatible.
        TokenType dataType = DecideDataType(&(variableRawExpression),
                                            &varComAssignStatement->expression);
        if (dataType == TokenType::UNKNOWN)
        {
            ReportError(
                std::format("Cannot assign value of type '{}' to variable "
                            "of type '{}'",
                            TokenDataTypeToString(
                                varComAssignStatement->expression->dataType),
                            varComAssignStatement->GetDataType()->GetLexeme()),
                varComAssignStatement->GetCodeErrString());
        }
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {

        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);

        Scope *scope = gZtoonArena.Allocate<Scope>(currentScope);
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

        if (ifStatement->GetExpression()->GetDataType() != TokenType::BOOL)
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

        if (elifStatement->GetExpression()->GetDataType() != TokenType::BOOL)
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
    }
    else if (dynamic_cast<WhileLoopStatement *>(statement))
    {
        WhileLoopStatement *whileStatement =
            dynamic_cast<WhileLoopStatement *>(statement);

        EvaluateAndAssignDataTypeToExpression(whileStatement->GetCondition());

        if (whileStatement->GetCondition()->GetDataType() != TokenType::BOOL)
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

            if (forLoopStatement->GetCondition()->GetDataType() !=
                TokenType::BOOL)
            {
                ReportError(
                    std::format("Expect expression '{}' to be boolean",
                                forLoopStatement->GetCondition()
                                    ->GetCodeErrString()
                                    .str),
                    forLoopStatement->GetCondition()->GetCodeErrString());
            }
        }

        // if (forLoopStatement->GetUpdate() != nullptr)
        // {
        //     AnalizeStatement(forLoopStatement->GetUpdate());
        // }

        AnalizeStatement(forLoopStatement->GetBlockStatement());
    }

    statementCurrentIndex++;
}

TokenType SemanticAnalyzer::DecideDataType(Expression **left,
                                           Expression **right)
{
    TokenType leftDataType = (*left)->GetDataType();
    TokenType rightDataType = (*right)->GetDataType();
    bool isLeftPrimary = dynamic_cast<PrimaryExpression *>((*left));
    bool isRightPrimary = dynamic_cast<PrimaryExpression *>((*right));
    if (leftDataType != rightDataType)
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
                (::IsInteger(rightPrimaryExpr->dataType) ||
                 ::IsFloat(rightPrimaryExpr->dataType)))
            {
                TokenType leftVarDataType = leftDataType;
                TokenType rightLiteralDataType = rightDataType;
                if (IsInteger(leftVarDataType) &&
                    IsInteger(rightLiteralDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *right;
                    castExpr->dataType = (*left)->GetDataType();
                    *right = castExpr;
                }
                else if (IsFloat(leftVarDataType) &&
                         IsFloat(rightLiteralDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *right;
                    castExpr->dataType = (*left)->GetDataType();
                    *right = castExpr;
                    return castExpr->dataType;
                }
                else
                {
                    // error
                    return TokenType::UNKNOWN;
                }
            }
            else if (rightPrimaryExpr->GetPrimary()->GetType() ==
                         TokenType::IDENTIFIER &&
                     leftPrimaryExpr->GetPrimary()->GetType() !=
                         TokenType::IDENTIFIER &&
                     (::IsInteger(leftPrimaryExpr->dataType) ||
                      ::IsFloat(leftPrimaryExpr->dataType)))
            {

                TokenType leftLiteralDataType = leftDataType;
                TokenType rightVarDataType = rightDataType;
                if (IsInteger(leftLiteralDataType) &&
                    IsInteger(rightVarDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *left;
                    castExpr->dataType = (*right)->GetDataType();
                    *right = castExpr;
                    return castExpr->dataType;
                }
                else if (IsFloat(leftLiteralDataType) &&
                         IsFloat(rightVarDataType))
                {
                    // cast
                    CastExpression *castExpr =
                        gZtoonArena.Allocate<CastExpression>();
                    castExpr->expression = *left;
                    castExpr->dataType = (*right)->GetDataType();
                    *right = castExpr;
                    return castExpr->dataType;
                }
                else
                {
                    // error
                    return TokenType::UNKNOWN;
                }
            }
            else
            {
                // error
                return TokenType::UNKNOWN;
            }
        }
        else
        {
            return TokenType::UNKNOWN;
        }
    }
    return leftDataType;
}

void SemanticAnalyzer::EvaluateAndAssignDataTypeToExpression(
    Expression *expression)
{

    if (dynamic_cast<TernaryExpression *>(expression))
    {
        TernaryExpression *ternaryExpr =
            dynamic_cast<TernaryExpression *>(expression);

        EvaluateAndAssignDataTypeToExpression(ternaryExpr->condition);
        EvaluateAndAssignDataTypeToExpression(ternaryExpr->trueExpr);
        EvaluateAndAssignDataTypeToExpression(ternaryExpr->falseExpr);

        if (ternaryExpr->condition->dataType != TokenType::BOOL)
        {
            ReportError("Expected expression to be boolean type",
                        ternaryExpr->GetCodeErrString());
        }

        TokenType type =
            DecideDataType(&ternaryExpr->trueExpr, &ternaryExpr->falseExpr);

        if (type == TokenType::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Expression '{}' and '{}' must be of the same type.",
                    ternaryExpr->trueExpr->GetCodeErrString().str,
                    ternaryExpr->falseExpr->GetCodeErrString().str),
                ternaryExpr->GetCodeErrString());
        }

        ternaryExpr->dataType = ternaryExpr->trueExpr->dataType;
    }
    else if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(
            binaryExpression->GetLeftExpression());
        EvaluateAndAssignDataTypeToExpression(
            binaryExpression->GetRightExpression());
        TokenType dataType =
            DecideDataType(&binaryExpression->left, &binaryExpression->right);
        if (dataType == TokenType::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Cannot perform '{}' on datatypes '{}' and '{}'",
                    binaryExpression->op->GetLexeme(),
                    TokenDataTypeToString(binaryExpression->left->dataType),
                    TokenDataTypeToString(binaryExpression->right->dataType)),
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
            if (!IsNumerical(binaryExpression->left->dataType))
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!IsNumerical(binaryExpression->right->dataType))
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            binaryExpression->dataType = dataType;
            break;
        }
        case TokenType::BITWISE_AND:
        case TokenType::BITWISE_OR:
        case TokenType::BITWISE_XOR:
        case TokenType::SHIFT_LEFT:
        case TokenType::SHIFT_RIGHT:
        {
            if (!IsInteger(binaryExpression->left->dataType))
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "integer type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!IsInteger(binaryExpression->right->dataType))
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "integer type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            binaryExpression->dataType = dataType;
            break;
        }
        case TokenType::EQUAL_EQUAL:
        case TokenType::EXCLAMATION_EQUAL:
        case TokenType::LESS:
        case TokenType::LESS_EQUAL:
        case TokenType::GREATER:
        case TokenType::GREATER_EQUAL:
        {

            if (!IsNumerical(binaryExpression->left->dataType) &&
                binaryExpression->left->dataType != TokenType::BOOL)
            {
                ReportError(std::format("Left expression of '{}' must be "
                                        "boolean or numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            else if (!IsNumerical(binaryExpression->right->dataType) &&
                     binaryExpression->right->dataType != TokenType::BOOL)
            {
                ReportError(std::format("Right expression of '{}' must be "
                                        "boolean or numerical type",
                                        binaryExpression->op->GetLexeme()),
                            binaryExpression->GetCodeErrString());
            }
            binaryExpression->dataType = TokenType::BOOL;
            break;
        }
        case TokenType::OR:
        case TokenType::AND:
        {
            if (binaryExpression->left->dataType != TokenType::BOOL)
            {
                ReportError(
                    std::format("Left expression of '{}' must be boolean type",
                                binaryExpression->op->GetLexeme()),
                    binaryExpression->GetCodeErrString());
            }
            else if (binaryExpression->right->dataType != TokenType::BOOL)
            {
                ReportError(
                    std::format("Right expression of '{}' must be boolean type",
                                binaryExpression->op->GetLexeme()),
                    binaryExpression->GetCodeErrString());
            }
            binaryExpression->dataType = TokenType::BOOL;
            break;
        }
        default:
        {
            binaryExpression->dataType = dataType;
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
        TokenType rightDataType =
            unaryExpression->GetRightExpression()->dataType;

        switch (unaryExpression->GetOperator()->GetType())
        {
        case TokenType::DASH:
        {
            // numerical
            if (!IsNumerical(rightDataType) && !IsSigned(rightDataType))
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::TILDE:
        {
            if (!IsInteger(rightDataType))
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                TokenDataTypeToString(rightDataType)),
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
                if (primaryExpr->primary->GetType() != TokenType::IDENTIFIER)
                {
                    ReportError(
                        std::format(
                            "Cannot perform unary operator '{}' on "
                            "non variables",
                            unaryExpression->GetOperator()->GetLexeme()),
                        unaryExpression->GetCodeErrString());
                }
                if (!IsNumerical(rightDataType))
                {
                    ReportError(
                        std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpression->GetOperator()->GetLexeme(),
                                    TokenDataTypeToString(rightDataType)),
                        unaryExpression->GetCodeErrString());
                }
                if (unaryExpression->IsPostfix())
                {
                    // Insert new variable assign statement with binary
                    VarAssignmentStatement *varAssignStatement =
                        gZtoonArena.Allocate<VarAssignmentStatement>();
                    Token *typeToken =
                        gZtoonArena.Allocate<Token>(rightDataType);
                    varAssignStatement->dataType = typeToken;
                    varAssignStatement->identifier = primaryExpr->GetPrimary();

                    BinaryExpression *binaryExpr =
                        gZtoonArena.Allocate<BinaryExpression>();
                    binaryExpr->left = primaryExpr;
                    PrimaryExpression *oneExpr =
                        gZtoonArena.Allocate<PrimaryExpression>();
                    oneExpr->dataType = rightDataType;

                    Token *oneToken = nullptr;
                    if (::IsInteger(rightDataType))
                    {
                        oneToken = gZtoonArena.Allocate<TokenLiteral<int32_t>>(
                            TokenType::INTEGER_LITERAL, 1);
                    }
                    else if (::IsFloat(rightDataType))
                    {
                        oneToken = gZtoonArena.Allocate<TokenLiteral<float>>(
                            TokenType::FLOAT_LITERAL, 1.0);
                    }
                    oneExpr->primary = oneToken;
                    binaryExpr->right = oneExpr;
                    binaryExpr->dataType = rightDataType;
                    binaryExpr->op =
                        unaryExpression->GetOperator()->type ==
                                TokenType::PLUS_PLUS
                            ? gZtoonArena.Allocate<Token>(TokenType::PLUS)
                            : gZtoonArena.Allocate<Token>(TokenType::DASH);
                    varAssignStatement->expression = binaryExpr;
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
            break;
        }
        case TokenType::PLUS:
        {
            // numerical
            if (!IsNumerical(rightDataType))
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::EXCLAMATION:
        {
            // numerical
            if (rightDataType != TokenType::BOOL)
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetCodeErrString());
            }
            break;
        }
        case TokenType::SIZEOF:
        {
            if (rightDataType == TokenType::UNKNOWN)
            {
                ReportError(
                    std::format("Cannot perform unary operator '{}' on "
                                "datatype '{}'.",
                                unaryExpression->GetOperator()->GetLexeme(),
                                TokenDataTypeToString(rightDataType)),
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
        unaryExpression->dataType = rightDataType;
    }
    else if (dynamic_cast<GroupingExpression *>(expression))
    {
        GroupingExpression *groupingExpression =
            dynamic_cast<GroupingExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(
            groupingExpression->GetExpression());
        TokenType exprDataType = groupingExpression->GetExpression()->dataType;
        groupingExpression->dataType = exprDataType;
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        EvaluateAndAssignDataTypeToExpression(castExpression->GetExpression());
        TokenType exprDataType = castExpression->GetExpression()->dataType;
        castExpression->dataType = castExpression->GetCastToType()->GetType();
        // TODO: is cast possible?
        // boolean are only catable to int types. it is one directional
        // thing. cannot cast int to bool. what types cannot be cast to
        // another?
        //

        // Only allow bool to bool cast for now.
        if (castExpression->dataType == TokenType::BOOL)
        {
            if (exprDataType != TokenType::BOOL)
            {
                ReportError(
                    std::format("Cannot cast datatype '{}' to datatype '{}'",
                                TokenDataTypeToString(exprDataType),
                                castExpression->GetCastToType()->GetLexeme()),
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
            primaryExpression->dataType = TokenType::I32;
            break;
        }
        case TokenType::FLOAT_LITERAL:
        {
            primaryExpression->dataType = TokenType::F32;
            break;
        }
        case TokenType::STRING_LITERAL:
        {
            // TODO: Need to handle pointers first.
            break;
        }
        case TokenType::CHARACTER_LITERAL:
        {
            primaryExpression->dataType = TokenType::U8;
            break;
        }
        case TokenType::TRUE:
        case TokenType::FALSE:
        {
            primaryExpression->dataType = TokenType::BOOL;
            break;
        }
        case TokenType::IDENTIFIER:
        {
            primaryExpression->dataType =
                currentScope
                    ->GetVariable(primaryExpression->GetPrimary()->GetLexeme(),
                                  primaryExpression->GetCodeErrString())
                    ->GetDataType()
                    ->GetType();
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
