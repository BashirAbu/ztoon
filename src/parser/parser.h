#pragma once
#include "lexer/lexer.h"
#include <string>
#include <unordered_map>
/*
  program -> statements* "EOF" ;
  statement -> var_decl_statement ";" ;
  var_decl_statement -> (IDENTIFIER ":" DATATYPE | "=" expression)
  | var_assignment_statement ;
  var_assignment_statement -> IDENTIFIER "="
  expression | statement_expr;

  var_compound_assignment_statement -> IDENTIFIER ( ("=" | "+=" | "-=" | "*=" |
  "/=", "%=", "&=", "^=", "|=", "<<=", ">>=") expression ;

  if_statement -> "if" expression (block | statement) (("else" "if"
  expression
  block * | "else" block ) ? ;

  block -> "{" statement* "}" ;


  statement_expr -> expression ;

  expression -> or ;

  or -> and ("||" and)* ;

  and -> bitwise_or ("&&" bitwise_or)* ;

  bitwise_or -> bitwise_xor ("|" bitwise_xor)* ;
  bitwise_xor -> bitwise_and ("^" bitwise_and)* ;
  bitwise_and -> ==__!= ("&" ==__!=)* ;
  ==__!= -> <__<=__>__>= (("==" | "!=")  <__<=__>__>=)* ;
  <__<=__>__>= -> shift (("<" | "<=" | ">" | ">=") sift)* ;
  shift -> term (("<<" | ">>") term)* ;
  term -> factor (("+" | "-") factor)* ;
  factor -> unary (("*" | "/" | "%") unary)* ;
  cast -> primary ("as" DATATYPE)*;
  unary -> ("-" | "--" | "+" | "++" | "!" | "~" | ( "sizeof" "(" expression ")"
  ) ) expression | cast ;
   primary -> INTEGER_LITERAL
  | FLOAT_LITERAL | STRING_LITERAL
  | CHARACTER_LITERAL | IDENTIFIER | "(" expression ")" ;

*/

extern std::unordered_map<Token const *, Token const *> identifierMap;

class Statement
{
  public:
    virtual ~Statement() {}
    virtual std::string PrettyString(std::string &prefix) = 0;
};

class VarDeclStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    Token const *GetIdentifier() const { return identifier; }
    Token const *GetDataType() const { return dataType; }
    class Expression *GetExpression() const { return expression; }

  private:
    Token const *identifier = nullptr;
    Token const *dataType = nullptr;
    // datatype
    class Expression *expression = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class VarCompoundAssignmentStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    Token const *GetIdentifier() const { return identifier; }
    Token const *GetDataType() const { return dataType; }
    Token const *GetCompoundAssignment() const { return compoundAssignment; }
    class Expression *GetExpression() const { return expression; }

  private:
    Token const *identifier = nullptr;
    Token const *dataType = nullptr;
    Token const *compoundAssignment = nullptr;
    class Expression *expression = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class VarAssignmentStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    Token const *GetIdentifier() const { return identifier; }
    Token const *GetDataType() const { return dataType; }
    class Expression *GetExpression() const { return expression; }

  private:
    Token const *identifier = nullptr;
    Token const *dataType = nullptr;
    class Expression *expression = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class ExpressionStatement : public Statement
{

  public:
    virtual std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }

  private:
    class Expression *expression = nullptr;
    friend class Parser;
};

class BlockStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    const std::vector<Statement *> &GetStatements() const { return statements; }

  private:
    std::vector<Statement *> statements;
    friend class Parser;
};

class IfStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }
    BlockStatement *GetBlockStatement() { return blockStatement; }
    const std::vector<Statement *> &GetNextElseIforElseStatements()
    {
        return nextElseIforElseStatements;
    }

    Token const *ifToken = nullptr;

  private:
    class Expression *expression = nullptr;
    BlockStatement *blockStatement;
    std::vector<Statement *> nextElseIforElseStatements;
    friend class Parser;
};

class ElseIfStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }
    BlockStatement *GetBlockStatement() { return blockStatement; }

    Token const *ifToken = nullptr;

  private:
    class Expression *expression = nullptr;
    BlockStatement *blockStatement;
    friend class Parser;
};
class ElseStatement : public Statement
{
  public:
    virtual std::string PrettyString(std::string &prefix) override;
    BlockStatement *GetBlockStatement() { return blockStatement; }

    Token const *elseToken = nullptr;

  private:
    BlockStatement *blockStatement;
    friend class Parser;
};

class Expression
{
  public:
    virtual ~Expression() {}
    virtual std::string PrettyString(std::string &prefix, bool isLeft) = 0;
    TokenType GetDataType() const { return dataType; }

  protected:
    TokenType dataType = TokenType::UNKNOWN;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class BinaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetLeftExpression() const { return left; }
    Expression *GetRightExpression() const { return right; }
    Token const *GetOperator() const { return op; }

  private:
    Expression *left = nullptr;
    Expression *right = nullptr;
    Token const *op = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class UnaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetRightExpression() const { return right; }
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

    class Expression *GetExpression() const { return expression; }

  private:
    Expression *expression = nullptr;
    friend class Parser;
};

class CastExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;

    class Expression *GetExpression() const { return expression; }
    Token const *GetCastToType() { return castToType; }

  private:
    Expression *expression = nullptr;
    Token const *castToType = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class PrimaryExpression : public Expression
{
  public:
    std::string PrettyString(std::string &prefix, bool isLeft) override;
    Token const *GetPrimary() const { return primary; }

  private:
    Token const *primary = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
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
    Statement *ParseVarCompundAssignmentStatement();
    Statement *ParseExpressionStatement();
    Statement *ParseBlockStatement();
    Statement *ParseIfStatement();
    Statement *ParseElseIfStatement();
    Statement *ParseElseStatement();

    Expression *ParseExpression();
    Expression *ParseAssignmentExpression();
    Expression *ParseORExpression();
    Expression *ParseANDExpression();
    Expression *ParseBitwiseORExpression();
    Expression *ParseBitwiseXORExpression();
    Expression *ParseBitwiseANDExpression();
    Expression *ParseEqualEqualNotEqualExpression();
    Expression *ParseLessGreaterExpression();
    Expression *ParseShiftExpression();

    Expression *ParseTermExpression();
    Expression *ParseFactorExpression();
    Expression *ParseCastExpression();
    Expression *ParseUnaryExpression();
    Expression *ParsePrimaryExpression();

    Expression *BuildBinaryExpression(Token const *op, Expression *left,
                                      Expression *right);

    bool Consume(TokenType type);
    Token const *Peek();
    Token const *PeekAhead(size_t steps);
    Token const *Prev();
    void Advance();
    size_t currentIndex = 0;
    const std::vector<Token *> tokens;
    std::vector<Statement *> statements;
};
