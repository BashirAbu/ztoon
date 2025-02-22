#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include <format>
Variable const *Scope::GetVariable(std::string name,
                                   Token const *tokenForErrorHandeling) const
{
    if (variablesMap.contains(name))
    {
        return variablesMap.at(name);
    }
    else
    {
        ReportError(std::format("Variable '{}' is not defined.", name),
                    tokenForErrorHandeling ? tokenForErrorHandeling : nullptr);
    }
    return nullptr;
}

void Scope::AddVariable(Variable *variable)
{
    if (variablesMap.contains(variable->GetName()))
    {
        Variable *alreadyDefinedVar = variablesMap[variable->GetName()];
        ReportError(
            std::format("Variable '{}' already defined at {}:{}.",
                        variable->GetName(),
                        alreadyDefinedVar->GetIdToken()->GetFilename(),
                        alreadyDefinedVar->GetIdToken()->GetLineNumber()),
            variable->GetIdToken());
    }
    else
    {
        variablesMap[variable->GetName()] = variable;
    }
}

SemanticAnalyzer::SemanticAnalyzer(const std::vector<Statement *> &statements)
    : statements(statements)
{
}
SemanticAnalyzer::~SemanticAnalyzer() {}
void SemanticAnalyzer::Analize()
{
    for (Statement *statement : statements)
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
                TokenType dataType = DecideDataType(
                    &(variableRawExpression), &varDeclStatement->expression);
                if (dataType == TokenType::UNKNOWN)
                {
                    ReportError(
                        std::format(
                            "Cannot assign value of type '{}' to variable "
                            "of type '{}'",
                            TokenDataTypeToString(
                                varDeclStatement->expression->dataType),
                            varDeclStatement->GetDataType()->GetLexeme()),
                        varDeclStatement->GetIdentifier());
                }
            }
            Variable *var = gZtoonArena.Allocate<Variable>(
                varDeclStatement->GetIdentifier()->GetLexeme(),
                varDeclStatement->GetDataType(),
                varDeclStatement->GetIdentifier());
            globalScope.AddVariable(var);
        }
        else if (dynamic_cast<VarAssignmentStatement *>(statement))
        {

            VarAssignmentStatement *varAssignmentStatement =
                dynamic_cast<VarAssignmentStatement *>(statement);

            Variable const *var = globalScope.GetVariable(
                varAssignmentStatement->GetIdentifier()->GetLexeme(),
                varAssignmentStatement->GetIdentifier());

            varAssignmentStatement->dataType = var->GetDataType();
            EvaluateAndAssignDataTypeToExpression(
                varAssignmentStatement->GetExpression());

            PrimaryExpression *varExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            varExpr->primary = varAssignmentStatement->GetIdentifier();
            varExpr->dataType =
                varAssignmentStatement->GetDataType()->GetType();
            Expression *variableRawExpression = varExpr;
            // check if types are compatible.
            TokenType dataType = DecideDataType(
                &(variableRawExpression), &varAssignmentStatement->expression);
            if (dataType == TokenType::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Cannot assign value of type '{}' to variable "
                        "of type '{}'",
                        TokenDataTypeToString(
                            varAssignmentStatement->expression->dataType),
                        varAssignmentStatement->GetDataType()->GetLexeme()),
                    varAssignmentStatement->GetIdentifier());
            }
        }
        else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
        {

            VarCompoundAssignmentStatement *varComAssignStatement =
                dynamic_cast<VarCompoundAssignmentStatement *>(statement);

            Variable const *var = globalScope.GetVariable(
                varComAssignStatement->GetIdentifier()->GetLexeme(),
                varComAssignStatement->GetIdentifier());
            varComAssignStatement->dataType = var->GetDataType();

            EvaluateAndAssignDataTypeToExpression(
                varComAssignStatement->GetExpression());
            PrimaryExpression *varExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            varExpr->primary = varComAssignStatement->GetIdentifier();
            varExpr->dataType = varComAssignStatement->GetDataType()->GetType();
            Expression *variableRawExpression = varExpr;
            // check if types are compatible.
            TokenType dataType = DecideDataType(
                &(variableRawExpression), &varComAssignStatement->expression);
            if (dataType == TokenType::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Cannot assign value of type '{}' to variable "
                        "of type '{}'",
                        TokenDataTypeToString(
                            varComAssignStatement->expression->dataType),
                        varComAssignStatement->GetDataType()->GetLexeme()),
                    varComAssignStatement->GetIdentifier());
            }
        }
        else if (dynamic_cast<ExpressionStatement *>(statement))
        {
            ExpressionStatement *exprStatement =
                dynamic_cast<ExpressionStatement *>(statement);
            EvaluateAndAssignDataTypeToExpression(
                exprStatement->GetExpression());
        }
    }
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
            return leftDataType;
        }
    }
    return leftDataType;
}

void SemanticAnalyzer::EvaluateAndAssignDataTypeToExpression(
    Expression *expression)
{
    if (dynamic_cast<BinaryExpression *>(expression))
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
                binaryExpression->op);
            return;
        }
        binaryExpression->dataType = dataType;
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
                    std::format(
                        "Cannot perform unary operator '{}' on datatype '{}'.",
                        unaryExpression->GetOperator()->GetLexeme(),
                        TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetOperator());
            }
            break;
        }
        case TokenType::TILDE:
        {
            if (!IsInteger(rightDataType))
            {
                ReportError(
                    std::format(
                        "Cannot perform unary operator '{}' on datatype '{}'.",
                        unaryExpression->GetOperator()->GetLexeme(),
                        TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetOperator());
            }
            break;
        }
        case TokenType::DASH_DASH:
        case TokenType::PLUS:
        case TokenType::PLUS_PLUS:
        {
            // numerical
            if (!IsNumerical(rightDataType))
            {
                ReportError(
                    std::format(
                        "Cannot perform unary operator '{}' on datatype '{}'.",
                        unaryExpression->GetOperator()->GetLexeme(),
                        TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetOperator());
            }
            break;
        }
        case TokenType::EXCLAMATION:
        {
            // numerical
            if (rightDataType != TokenType::BOOL)
            {
                ReportError(
                    std::format(
                        "Cannot perform unary operator '{}' on datatype '{}'.",
                        unaryExpression->GetOperator()->GetLexeme(),
                        TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetOperator());
            }
            break;
        }
        case TokenType::SIZEOF:
        {
            if (rightDataType == TokenType::UNKNOWN)
            {
                ReportError(
                    std::format(
                        "Cannot perform unary operator '{}' on datatype '{}'.",
                        unaryExpression->GetOperator()->GetLexeme(),
                        TokenDataTypeToString(rightDataType)),
                    unaryExpression->GetOperator());
            }
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
        // boolean are only catable to int types. it is one directional thing.
        // cannot cast int to bool.
        // what types cannot be cast to another?
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
                    castExpression->GetCastToType());
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
                globalScope
                    .GetVariable(primaryExpression->GetPrimary()->GetLexeme(),
                                 primaryExpression->GetPrimary())
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
        ReportError("This expression is not supported.", nullptr);
    };
}
