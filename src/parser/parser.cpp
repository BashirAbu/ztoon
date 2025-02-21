#include "error_report.h"
#include "lexer/lexer.h"
#include "parser.h"
#include <format>
#include <iostream>
std::unordered_map<Token const *, Token const *> identifierMap;

bool Parser::Consume(TokenType type)
{
    if (type == Peek()->GetType())
    {
        Advance();
        return true;
    }
    else
    {
        return false;
    }
}
Token const *Parser::Peek()
{
    if (currentIndex < tokens.size())
    {
        return tokens.at(currentIndex);
    }
    return nullptr;
}
Token const *Parser::PeekAhead(size_t steps)
{
    if ((currentIndex + steps) < tokens.size())
    {
        return tokens.at(currentIndex + steps);
    }
    return nullptr;
}
void Parser::Advance()
{
    if ((currentIndex + 1) < tokens.size())
        currentIndex++;
}

Token const *Parser::Prev()
{
    if ((currentIndex - 1) < tokens.size())
        return tokens.at(currentIndex - 1);
    else
        return nullptr;
}

Parser::Parser(const std::vector<Token *> &tokens) : tokens(tokens) {}

Parser::~Parser() {}

const std::vector<Statement *> &Parser::Parse()
{
    while (Peek()->GetType() != TokenType::END_OF_FILE)
    {
        ParseStatement();
    }
    return statements;
}

Statement *Parser::ParseStatement()
{
    Statement *statement = ParseVarDeclStatement();
    if (Consume(TokenType::SEMICOLON))
    {
        return statement;
    }
    ReportError("Expected ';' after statement.", Peek());
    return nullptr;
}
Statement *Parser::ParseVarDeclStatement()
{
    if (Peek()->GetType() == TokenType::IDENTIFIER &&
        PeekAhead(1)->GetType() == TokenType::COLON)
    {
        Advance();
        VarDeclStatement *varDeclStatement =
            gZtoonArena.Allocate<VarDeclStatement>();
        statements.push_back(varDeclStatement);
        varDeclStatement->identifier = Prev();
        if (Consume(TokenType::COLON))
        {
            if (IsDataType(Peek()->GetType()))
            {
                Advance();
                varDeclStatement->dataType = Prev();
                identifierMap[varDeclStatement->identifier] =
                    varDeclStatement->dataType;
                if (Consume(TokenType::EQUAL))
                {
                    varDeclStatement->expression = ParseExpression();
                    if (!varDeclStatement->expression)
                    {
                        ReportError("Expected expression after '='.", Peek());
                        return nullptr;
                    }
                }
                return varDeclStatement;
            }
        }
    }
    return ParseVarAssignmentStatement();
}

Statement *Parser::ParseVarAssignmentStatement()
{

    if (Peek()->GetType() == TokenType::IDENTIFIER &&
        PeekAhead(1)->GetType() == TokenType::EQUAL)
    {
        Advance();
        VarAssignmentStatement *statement =
            gZtoonArena.Allocate<VarAssignmentStatement>();
        statements.push_back(statement);
        statement->identifier = Prev();
        Advance();
        statement->expression = ParseExpression();
        return statement;
    }
    else
    {
        return ParseVarCompundAssignmentStatement();
    }
}
Statement *Parser::ParseVarCompundAssignmentStatement()
{
    if (Peek()->GetType() == TokenType::IDENTIFIER &&
        IsCompoundAssignment(PeekAhead(1)->GetType()))
    {
        Advance();
        VarCompoundAssignmentStatement *statement =
            gZtoonArena.Allocate<VarCompoundAssignmentStatement>();
        statements.push_back(statement);
        statement->identifier = Prev();
        PrimaryExpression *left = gZtoonArena.Allocate<PrimaryExpression>();

        left->primary = Prev();
        left->dataType = TokenType::UNKNOWN; // TODO: do types.
        Advance();
        statement->compoundAssignment = Prev();
        Expression *right = ParseExpression();
        if (!right)
            ReportError(std::format("Expect expression after '{}'",
                                    Peek()->GetLexeme()),
                        Peek());
        BinaryExpression *binaryExpression =
            gZtoonArena.Allocate<BinaryExpression>();
        binaryExpression->left = left;
        binaryExpression->right = right;

        if (!binaryExpression->right)
            ReportError(std::format("Expect expression after '{}'",
                                    binaryExpression->op->GetLexeme()),
                        binaryExpression->op);
        switch (statement->compoundAssignment->GetType())
        {
        case TokenType::PLUS_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::PLUS);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PLUS;
            op->lexeme = "+";
            binaryExpression->op = op;
            break;
        }
        case TokenType::DASH_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::DASH);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::DASH;
            op->lexeme = "-";
            binaryExpression->op = op;
            break;
        }
        case TokenType::ASTERISK_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::ASTERISK);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::ASTERISK;
            op->lexeme = "*";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SLASH_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SLASH);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SLASH;
            op->lexeme = "/";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_OR_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_OR);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::BITWISE_OR;
            op->lexeme = "|";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_AND_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_AND);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PLUS;
            op->lexeme = "&";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_XOR_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_XOR);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::BITWISE_XOR;
            op->lexeme = "^";
            binaryExpression->op = op;
            break;
        }
        case TokenType::PERCENTAGE_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::PERCENTAGE);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PERCENTAGE;
            op->lexeme = "%%";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SHIFT_LEFT_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SHIFT_LEFT);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SHIFT_LEFT;
            op->lexeme = "<<";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SHIFT_RIGHT_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SHIFT_RIGHT);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SHIFT_RIGHT;
            op->lexeme = ">>";
            binaryExpression->op = op;
            break;
        }
        default:
        {
            break;
        }
        }
        statement->expression = binaryExpression;
        return statement;
    }
    else
    {
        return ParseExpressionStatement();
    }
}
Statement *Parser::ParseExpressionStatement()
{
    ExpressionStatement *es = gZtoonArena.Allocate<ExpressionStatement>();
    statements.push_back(es);
    es->expression = ParseExpression();
    return es;
}

Expression *Parser::BuildBinaryExpression(Token const *op, Expression *left,
                                          Expression *right)
{
    BinaryExpression *expr = gZtoonArena.Allocate<BinaryExpression>();
    if (!left)
        ReportError(
            std::format("Expect expression before '{}'", op->GetLexeme()), op);
    if (!right)
        ReportError(
            std::format("Expect expression after '{}'", op->GetLexeme()), op);

    expr->left = left;
    expr->right = right;
    expr->op = op;
    return expr;
}

Expression *Parser::ParseExpression() { return ParseORExpression(); }

Expression *Parser::ParseORExpression()
{
    Expression *expr = ParseANDExpression();
    while (Consume(TokenType::OR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseANDExpression());
    }
    return expr;
}
Expression *Parser::ParseANDExpression()
{
    Expression *expr = ParseBitwiseORExpression();
    while (Consume(TokenType::AND))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseORExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseORExpression()
{

    Expression *expr = ParseBitwiseXORExpression();
    while (Consume(TokenType::BITWISE_OR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseXORExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseXORExpression()
{

    Expression *expr = ParseBitwiseANDExpression();
    while (Consume(TokenType::BITWISE_XOR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseANDExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseANDExpression()
{

    Expression *expr = ParseEqualEqualNotEqualExpression();
    while (Consume(TokenType::BITWISE_AND))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr,
                                     ParseEqualEqualNotEqualExpression());
    }
    return expr;
}
Expression *Parser::ParseEqualEqualNotEqualExpression()
{

    Expression *expr = ParseLessGreaterExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::EQUAL_EQUAL,
                      TokenType::EXCLAMATION_EQUAL))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseLessGreaterExpression());
    }
    return expr;
}
Expression *Parser::ParseLessGreaterExpression()
{
    Expression *expr = ParseShiftExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::LESS, TokenType::LESS_EQUAL,
                      TokenType::GREATER, TokenType::GREATER_EQUAL))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseShiftExpression());
    }
    return expr;
}
Expression *Parser::ParseShiftExpression()
{

    Expression *expr = ParseTermExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::SHIFT_LEFT,
                      TokenType::SHIFT_RIGHT))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseTermExpression());
    }
    return expr;
}
Expression *Parser::ParseTermExpression()
{
    Expression *expr = ParseFactorExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::PLUS, TokenType::DASH))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseFactorExpression());
    }
    return expr;
}

Expression *Parser::ParseFactorExpression()
{
    Expression *expr = ParseUnaryExpression();

    while (TokenMatch(Peek()->GetType(), TokenType::ASTERISK, TokenType::SLASH))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseUnaryExpression());
    }
    return expr;
}

Expression *Parser::ParseUnaryExpression()
{
    if (TokenMatch(Peek()->GetType(), TokenType::DASH, TokenType::DASH_DASH,
                   TokenType::PLUS, TokenType::PLUS_PLUS,
                   TokenType::EXCLAMATION, TokenType::TILDE, TokenType::SIZEOF))
    {
        Advance();
        UnaryExpression *unaryExpr = gZtoonArena.Allocate<UnaryExpression>();
        unaryExpr->op = Prev();
        if ((Prev()->GetType() == TokenType::SIZEOF) &&
            Consume(TokenType::LEFT_PAREN))
        {
            unaryExpr->right = ParseCastExpression();
            if (!unaryExpr->right)
                ReportError(std::format("Expect expression after '{}'",
                                        unaryExpr->op->GetLexeme()),
                            unaryExpr->op);
            if (Consume(TokenType::RIGHT_PAREN))
            {
                return unaryExpr;
            }
            else
            {
                ReportError("Expect ')' after expression.", Prev());
                return nullptr;
            }
        }
        unaryExpr->right = ParseCastExpression();
        return unaryExpr;
    }
    return ParseCastExpression();
}

Expression *Parser::ParseCastExpression()
{
    Expression *expr = ParsePrimaryExpression();

    while (Consume(TokenType::AS))
    {
        CastExpression *castExpr = gZtoonArena.Allocate<CastExpression>();
        castExpr->expression = expr;
        if (IsDataType(Peek()->GetType()))
        {
            castExpr->dataType = Peek()->GetType();
            Advance();
        }
        else
        {
            ReportError("Expect datatype after 'as'.", Peek());
        }
        castExpr->castToType = Prev();
        expr = castExpr;
    }

    return expr;
}
Expression *Parser::ParsePrimaryExpression()
{
    Expression *retExpr = nullptr;
    switch (Peek()->GetType())
    {
    case TokenType::INTEGER_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::FLOAT_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::STRING_LITERAL:
    {

        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::CHARACTER_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::TRUE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::FALSE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::IDENTIFIER:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::LEFT_PAREN:
    {
        Consume(TokenType::LEFT_PAREN);
        GroupingExpression *gExpr = gZtoonArena.Allocate<GroupingExpression>();
        gExpr->expression = ParseExpression();
        if (Consume(TokenType::RIGHT_PAREN))
        {
            retExpr = gExpr;
        }
        else
        {
            ReportError("Expected ')' after expression.", Prev());
        }
        break;
    }
    default:
    {
        break;
    }
    }
    return retExpr;
}

void Parser::PrettyPrintAST()
{
    for (Statement *s : statements)
    {
        std::string astStr = s->PrettyString();

        std::cout << astStr << std::endl;
    }
}

std::string VarDeclStatement::PrettyString()
{
    std::string str = "";
    std::string prefix = "";
    str += "|_(=)\n";
    prefix += "    ";
    if (expression)
    {
        str += expression->PrettyString(prefix, true);
    }
    str += prefix;
    str += "|_";
    str += "var_decl(" + identifier->GetLexeme() + " (" +
           dataType->GetLexeme() + ")" + ")\n";
    return str;
}

std::string VarAssignmentStatement::PrettyString()
{
    std::string str = "";
    std::string prefix = "";
    str += "|_(=)\n";
    prefix += "    ";
    str += expression ? expression->PrettyString(prefix, true) : "";
    str += prefix;
    str += "|_";
    str += "var_assign(" + identifier->GetLexeme() + " (" +
           TokenDataTypeToString(dataType ? dataType->GetType()
                                          : TokenType::UNKNOWN) +
           ")" + ")\n";
    return str;
}

std::string VarCompoundAssignmentStatement::PrettyString()
{
    std::string str = "";
    std::string prefix = "";
    str += "|_(=)\n";
    prefix += "    ";
    str += expression->PrettyString(prefix, true);
    str += prefix;
    str += "|_";
    str += "var_assign(" + identifier->GetLexeme() + " (datatype" + ")" + ")\n";
    return str;
}

std::string ExpressionStatement::PrettyString()
{
    std::string str = "";
    std::string prefix = "";
    str += expression ? expression->PrettyString(prefix, true) : "";
    return str;
}

std::string BinaryExpression::PrettyString(std::string &prefix, bool isLeft)
{
    std::string str = "";
    str += prefix;
    str += isLeft ? "|-" : "|_";
    str += (std::string) "(" + op->GetLexeme() + ")\n";
    prefix += isLeft ? "|   " : "    ";
    str += left->PrettyString(prefix, true);
    str += right->PrettyString(prefix, false);
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    return str;
}

std::string UnaryExpression::PrettyString(std::string &prefix, bool isLeft)
{
    std::string str = "";
    str += prefix;
    str += isLeft ? "|-" : "|_";
    str += (std::string) "(" + op->GetLexeme() + ")\n";
    prefix += isLeft ? "|   " : "    ";
    str += right->PrettyString(prefix, false);
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();

    return str;
}

std::string GroupingExpression::PrettyString(std::string &prefix, bool isLeft)
{
    std::string str = "";
    str += prefix;
    str += isLeft ? "|-" : "|_";
    str += "()\n";
    prefix += isLeft ? "|   " : "    ";
    str += expression ? expression->PrettyString(prefix, false) : "";
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    return str;
}

std::string CastExpression::PrettyString(std::string &prefix, bool isLeft)
{
    std::string str = "";
    str += prefix;
    str += isLeft ? "|-" : "|_";
    str += (std::string) "as" + "\n";
    prefix += isLeft ? "|   " : "    ";
    str += prefix;
    str += "|-";
    str += TokenDataTypeToString(dataType) + "\n";
    str += expression ? expression->PrettyString(prefix, true) : "";
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    prefix.pop_back();
    return str;
}

std::string PrimaryExpression::PrettyString(std::string &prefix, bool isleft)
{
    std::string str = "";

    str += prefix;
    str += isleft ? "|-" : "|_";
    str += primary->GetLexeme() + "\n";
    return str;
}
