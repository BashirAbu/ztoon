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

  expression -> term (("+" \ "-") term)* ;
  term -> factor (("*" \ "/") factor)* ;
  factor -> cast (("*" \ "/") cast)* ;
  cast -> primary ("as" DATATYPE)*;
  primary -> INTEGER_LITERAL \ FLOAT_LITERAL \ STRING_LITERAL
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

  private:
    Expression *left = nullptr;
    Expression *right = nullptr;
    Token const *op = nullptr;
    friend class Parser;
};

class GroupingExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

  private:
    Expression *expression = nullptr;
    friend class Parser;
};

class CastExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

  private:
    Expression *expression = nullptr;
    Token const *dataType = nullptr;
    friend class Parser;
};

class PrimaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

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

    Statement *ParseStatement();
    Statement *ParseVarDeclStatement();
    Statement *ParseVarAssignmentStatement();
    Statement *ParseExpressionStatement();

    Expression *ParseExpression();
    Expression *ParseTermExpression();
    Expression *ParseFactorExpression();
    Expression *ParseCastExpression();
    Expression *ParsePrimaryExpression();

  private:
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

/*
(1 + 2) * 3;
\_ Grouping '()'
    \_+
        \- 1
        \_ 2


*/
