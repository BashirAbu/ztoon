#pragma once
#include "lexer/lexer.h"
#include <string>
#include <unordered_map>
/*
  program -> statements* "EOF" ;
  statement -> var_decl_statement ";" ;
  var_decl_statement -> (IDENTIFIER ":" DATATYPE \ "=" expression)
  \ var_assignment_statement ;
  var_assignment_statement -> IDENTIFIER "="
  expression | statement_expr;
  statement_expr -> expression ;

  expression ->  or ;
  or -> and ("||" and)* ;
  and -> bitwise_or ("&&" bitwise_or)* ;
  bitwise_or -> bitwise_xor ("|" bitwise_xor)* ;
  bitwise_xor -> bitwise_and ("^" bitwise_and)* ;
  bitwise_and -> ==__!= ("&" ==__!=)* ;
  ==__!= -> <__<=__>__>= (("==" | "!=")  <__<=__>__>=)* ;
  <__<=__>__>= -> shift (("<" | "<=" | ">" | ">=") sift)* ;
  shift -> term (("<<" | ">>") term)* ;
  term -> factor (("+" \ "-") factor)* ;
  factor -> unary (("*" \ "/" \ "%") unary)* ;
  unary -> ("-" \ "--" \ "+" \ "++" \ "!" \ "~" \ ( "sizeof" "(" expression ")"
  ) ) expression \ cast
  cast -> primary ("as" DATATYPE)*; primary -> INTEGER_LITERAL
  \ FLOAT_LITERAL \ STRING_LITERAL
  \ CHARACTER_LITERAL \ IDENTIFIER \ "(" expression ")" ;

*/

extern std::unordered_map<Token const *, Token const *> identifierMap;

class Statement
{
  public:
    virtual ~Statement() {}
    virtual std::string PrettyString() = 0;
};

class VarDeclStatement : public Statement
{
  public:
    virtual std::string PrettyString() override;
    Token const *GetIdentifier() const { return identifier; }
    Token const *GetDataType() const { return dataType; }
    class Expression const *GetExpression() const { return expression; }

  private:
    Token const *identifier = nullptr;
    Token const *dataType = nullptr;
    // datatype
    class Expression *expression = nullptr;
    friend class Parser;
};

class VarAssignmentStatement : public Statement
{
  public:
    virtual std::string PrettyString() override;
    Token const *GetIdentifier() const { return identifier; }
    Token const *GetDataType() const { return dataType; }
    class Expression const *GetExpression() const { return expression; }

  private:
    Token const *identifier = nullptr;
    Token const *dataType = nullptr;
    class Expression *expression = nullptr;
    friend class Parser;
};

class ExpressionStatement : public Statement
{

  public:
    virtual std::string PrettyString() override;
    class Expression const *GetExpression() const { return expression; }

  private:
    class Expression *expression = nullptr;
    friend class Parser;
};

class Expression
{
  public:
    virtual ~Expression() {}
    virtual std::string PrettyString(std::string &prefix, bool isLeft) = 0;

  private:
    friend class Parser;
};

class BinaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression const *GetLeftExpression() const { return left; }
    Expression const *GetRightExpression() const { return right; }
    Token const *GetOperator() const { return op; }

  private:
    Expression *left = nullptr;
    Expression *right = nullptr;
    Token const *op = nullptr;
    friend class Parser;
};

class UnaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression const *GetRightExpression() const { return right; }
    Token const *GetOperator() const { return op; }

  private:
    Expression *right = nullptr;
    Token const *op = nullptr;
    friend class Parser;
};

class GroupingExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

    class Expression const *GetExpression() const { return expression; }

  private:
    Expression *expression = nullptr;
    friend class Parser;
};

class CastExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

    class Expression const *GetExpression() const { return expression; }
    Token const *GetDataType() const { return dataType; }

  private:
    Expression *expression = nullptr;
    Token const *dataType = nullptr;
    friend class Parser;
};

class PrimaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Token const *GetPrimary() const { return primary; }
    TokenType GetDataType() const { return dataType; }

  private:
    Token const *primary = nullptr;
    TokenType dataType = TokenType::UNKNOWN;
    friend class Parser;
};

class Parser
{
  public:
    Parser(const std::vector<Token *> &tokens);
    ~Parser();
    void PrettyPrintAST();
    const std::vector<Statement *> &Parse();

  private:
    Statement *ParseStatement();
    Statement *ParseVarDeclStatement();
    Statement *ParseVarAssignmentStatement();
    Statement *ParseExpressionStatement();

    Expression *ParseExpression();
    Expression *ParseTermExpression();
    Expression *ParseFactorExpression();
    Expression *ParseUnaryExpression();
    Expression *ParseCastExpression();
    Expression *ParsePrimaryExpression();
    bool Consume(TokenType type);
    Token const *Peek();
    Token const *PeekAhead(size_t steps);
    Token const *Prev();
    void Advance();
    size_t currentIndex = 0;
    const std::vector<Token *> tokens;
    std::vector<Expression *> expressions;
    std::vector<Statement *> statements;
};
