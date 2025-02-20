#include "error_report.h"
#include "lexer/lexer.h"
#include "parser.h"
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
                    return varDeclStatement;
                }
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

Expression *Parser::ParseExpression() { return ParseTermExpression(); }

Expression *Parser::ParseTermExpression()
{
    Expression *expr = ParseFactorExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::PLUS, TokenType::DASH))
    {
        Advance();
        BinaryExpression *binaryExpression =
            gZtoonArena.Allocate<BinaryExpression>();
        expressions.push_back(binaryExpression);
        binaryExpression->left = expr;
        binaryExpression->op = Prev();
        binaryExpression->right = ParseFactorExpression();
        expr = binaryExpression;
    }
    return expr;
}

Expression *Parser::ParseFactorExpression()
{
    Expression *expr = ParseUnaryExpression();

    while (TokenMatch(Peek()->GetType(), TokenType::ASTERISK, TokenType::SLASH))
    {
        Advance();
        BinaryExpression *binaryExpression =
            gZtoonArena.Allocate<BinaryExpression>();
        expressions.push_back(binaryExpression);
        binaryExpression->left = expr;
        binaryExpression->op = Prev();
        binaryExpression->right = ParseUnaryExpression();
        expr = binaryExpression;
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
        expressions.push_back(unaryExpr);
        unaryExpr->op = Prev();
        if ((Prev()->GetType() == TokenType::SIZEOF) &&
            Consume(TokenType::LEFT_PAREN))
        {
            unaryExpr->right = ParseCastExpression();

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
        expressions.push_back(castExpr);
        castExpr->expression = expr;
        if (IsDataType(Peek()->GetType()))
        {
            Advance();
            castExpr->dataType = Prev();
        }
        else
        {
            ReportError("Expect datatype after 'as'.", Peek());
        }
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
        expressions.push_back(expr);
        expr->primary = Prev();
        expr->dataType = TokenType::U64;
        retExpr = expr;
        break;
    }
    case TokenType::FLOAT_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        expr->dataType = TokenType::F64;
        retExpr = expr;
        break;
    }
    case TokenType::STRING_LITERAL:
    {

        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::CHARACTER_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::TRUE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::FALSE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::IDENTIFIER:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expressions.push_back(expr);
        expr->primary = Prev();
        retExpr = expr; // TODO: Check if you need to assing a type here?
        break;
    }
    case TokenType::LEFT_PAREN:
    {
        Consume(TokenType::LEFT_PAREN);
        GroupingExpression *gExpr = gZtoonArena.Allocate<GroupingExpression>();
        expressions.push_back(gExpr);
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
    str += dataType->GetLexeme() + "\n";
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
