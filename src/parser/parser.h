#pragma once
#include "lexer/lexer.h"
#include <cmath>
#include <format>
#include <string>
#include <unordered_map>
/*
  program -> declaration* "EOF" ;
  declaration -> var_decl_statement* | fn_statement* | struct_decl_statement* |
  enum_decl_statement* | union_decl_statement* ;
    statement -> var_decl_statement ";" ;
    var_decl_statement -> ( IDENTIFIER ":" DATATYPE ("="
  expression)?) | var_assignment_statement ;
  var_assignment_statement ->
  lvalue_expression "=" expression | statement_expr;

  var_compound_assignment_statement -> lvalue_expression ( ("=" | "+=" | "-=" |
  "*=" |
  "/=", "%=", "&=", "^=", "|=", "<<=", ">>=") expression ;


  struct_decl_statementi -> struct IDENTIFIER "{" ( var_decl_statement ";" )*?
  "}"


  while_loop_statement -> "while" expression block ;

  for_loop_statement -> "for" ( var_decl_statement | var_assignment_statment |
  statement_expr )* ";" expression* ";" (var_assignment_statment |
  statement_expr )* block

  if_statement -> "if" expression (block | statement) (("else" "if"
  expression
  block * | "else" block ) ? ;

  block -> "{" statement* "}" ;

  fn_statement -> "fn" IDENTIFIER "("  (var_decl_statement ","?)*  ")" ("->"
  DATATYPE)? block? ;

  statement_expr -> expression ;

  expression -> lambda_expression ;
  lambda->expression -> (fn "(" (var_decl_statement ","?)* ")" block) |
  ternary_expression ;
  ternary_expression -> (expression ? expression :
  expression) | or ; or -> and ("||" and)* ;

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

   fn_call_expression -> IDENTIFIER "("  expression*  ")" ;

   primary -> INTEGER_LITERAL
  | FLOAT_LITERAL | STRING_LITERAL
  | CHARACTER_LITERAL | IDENTIFIER | "(" expression ")" | fn_call_expression |
  lvalue_expression | ref_expression | deref_expression;
    ref_expression -> "&" ( IDENTIFIER | deref_expression );
    deref_expression -> "*" expression ;
    lvalue_expression -> IDENTIFIER | deref_expression; //only ptr type can be
  derefrenced.

    DATA_TYPE -> ( "readonly"? 'DATATYPE' ) | fn args* ("->" DATATYPE)? ;
*/

struct CodeErrString
{
    Token const *firstToken;
    std::string str;
};
class DataTypeToken
{

  public:
    Token const *GetDataType() const { return dataType; }

    CodeErrString GetCodeErrString()
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format("{}", es.firstToken->GetLexeme());
        return es;
    }

    Token const *GetFirstToken() const
    {
        return readOnly ? readOnly : dataType;
    }
    std::string ToString();

  private:
    Token const *dataType = nullptr;
    Token const *readOnly = nullptr;

    class FnStatement *fnStatement = nullptr;

    struct ArrayDesc
    {
        size_t arrSize = 0;
        Token const *token = nullptr;
        class Expression *arraySizeExpr = nullptr;
        DataTypeToken *dataTypeToken = nullptr;
    };
    ArrayDesc *arrayDesc = nullptr;
    struct PointerDesc
    {
        Token const *token = nullptr;
        DataTypeToken *dataTypeToken = nullptr;
    };

    PointerDesc *pointerDesc = nullptr;

    friend class Parser;
    friend class DataType;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class PointerDataType;

  public:
    class FnStatement *GetFnStatement() { return fnStatement; }
    ArrayDesc *GetArraDesc() { return arrayDesc; }
};

class Expression
{
  public:
    virtual ~Expression() {}
    //// virtual //std::string PrettyString(std::string &prefix, bool isLeft) =
    /// 0;
    virtual CodeErrString GetCodeErrString() = 0;
    virtual Token const *GetFirstToken() const = 0;
    bool IsLValue() { return isLvalue; }

  protected:
    bool isLvalue = false;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class Statement
{
  public:
    virtual ~Statement() {}
    //// virtual //std::string PrettyString(std::string &prefix) = 0;
    virtual CodeErrString GetCodeErrString() = 0;
};
class EmptyStatement : public Statement
{

  public:
    ~EmptyStatement() {}
    // std::string PrettyString(std::string &prefix) { return ""; }
    CodeErrString GetCodeErrString()
    {
        CodeErrString err = {};
        return err;
    }
};

class BreakStatement : public Statement
{

  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str = "break";
        return ces;
    }

  private:
    Token const *token = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class ContinueStatement : public Statement
{

  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str = "continue";
        return ces;
    }

  private:
    Token const *token = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class VarDeclStatement : public Statement
{
  public:
    //// virtual //std::string PrettyString(std::string &prefix) override;
    Token const *GetIdentifier() const { return identifier; }
    DataTypeToken *GetDataType() { return dataTypeToken; }
    Expression *GetExpression() const { return expression; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = identifier;
        ces.str = expression
                      ? std::format("{} : {} = {}", identifier->GetLexeme(),
                                    dataTypeToken->ToString(),
                                    expression->GetCodeErrString().str)
                      : std::format("{} : {}", identifier->GetLexeme(),
                                    dataTypeToken->ToString());
        return ces;
    }

    bool IsParamter() { return isParamter; }
    bool IsGlobal() { return isGlobal; }

  private:
    Token const *identifier = nullptr;
    DataTypeToken *dataTypeToken = nullptr;
    // datatype
    Expression *expression = nullptr;

    bool isParamter = false;
    bool isGlobal = false;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class VarCompoundAssignmentStatement : public Statement
{
  public:
    //// virtual //std::string PrettyString(std::string &prefix) override;
    Expression *GetLValue() { return lValue; }
    Expression *GetRValue() const { return rValue; }
    Token const *GetCompoundAssignment() const { return compoundAssignment; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = lValue->GetFirstToken();
        ces.str = std::format("{} {} {}", lValue->GetCodeErrString().str,
                              compoundAssignment->GetLexeme(),
                              rValue->GetCodeErrString().str);
        return ces;
    }

  private:
    Expression *lValue = nullptr;
    Token const *compoundAssignment = nullptr;
    class Expression *rValue = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class VarAssignmentStatement : public Statement
{
  public:
    //// virtual //std::string PrettyString(std::string &prefix) override;
    Expression *GetLValue() { return lValue; }
    Expression *GetRValue() const { return rValue; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = lValue->GetFirstToken();
        ces.str = std::format("{} = {}", lValue->GetCodeErrString().str,
                              rValue->GetCodeErrString().str);
        return ces;
    }

  private:
    Expression *lValue = nullptr;
    class Expression *rValue = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class StructStatement : public Statement
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str = std::format("struct {}\n", identifier->GetLexeme());
        ces.str += "{\n";
        for (auto field : fields)
        {
            ces.str += field->GetCodeErrString().str;
            ces.str += ";\n";
        }

        ces.str += "}";

        return ces;
    }

  private:
    Token const *token = nullptr;
    Token const *identifier = nullptr;
    std::vector<VarDeclStatement *> fields;

    friend class Parser;
    friend class SemanticAnalyzer;
};
class ExpressionStatement : public Statement
{

  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = expression->GetCodeErrString();
        return ces;
    }

  private:
    class Expression *expression = nullptr;
    friend class Parser;
};

class BlockStatement : public Statement
{
  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    std::vector<Statement *> &GetStatements() { return statements; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces{};
        ces.firstToken = statements.size() > 0
                             ? statements[0]->GetCodeErrString().firstToken
                             : nullptr;
        ces.str += "{\n";
        for (Statement *s : statements)
        {
            ces.str += std::format("    {}\n", s->GetCodeErrString().str);
        }
        ces.str += "}\n";
        return ces;
    }
    size_t index = 0;

  private:
    std::vector<Statement *> statements;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class IfStatement : public Statement
{
  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }
    BlockStatement *GetBlockStatement() { return blockStatement; }
    const std::vector<Statement *> &GetNextElseIforElseStatements()
    {
        return nextElseIforElseStatements;
    }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = ifToken;

        ces.str = std::format("if {}\n{}", expression->GetCodeErrString().str,
                              blockStatement->GetCodeErrString().str);

        return ces;
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
    // virtual //std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }
    BlockStatement *GetBlockStatement() { return blockStatement; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = ifToken;

        ces.str =
            std::format("else if {}\n{}", expression->GetCodeErrString().str,
                        blockStatement->GetCodeErrString().str);

        return ces;
    }
    Token const *ifToken = nullptr;

  private:
    class Expression *expression = nullptr;
    BlockStatement *blockStatement;
    friend class Parser;
};
class ElseStatement : public Statement
{
  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    BlockStatement *GetBlockStatement() { return blockStatement; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = elseToken;

        ces.str =
            std::format("else {}", blockStatement->GetCodeErrString().str);

        return ces;
    }
    Token const *elseToken = nullptr;

  private:
    BlockStatement *blockStatement;
    friend class Parser;
};

class WhileLoopStatement : public Statement
{
  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    BlockStatement *GetBlockStatement() { return blockStatement; }
    Expression *GetCondition() { return condition; }
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = whileToken;

        ces.str = std::format("while {} {}", condition->GetCodeErrString().str,
                              blockStatement->GetCodeErrString().str);
        return ces;
    }

  private:
    Token const *whileToken = nullptr;
    Expression *condition = nullptr;
    BlockStatement *blockStatement = nullptr;
    friend class Parser;
};

class ForLoopStatement : public Statement
{
  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    BlockStatement *GetBlockStatement() { return blockStatement; }
    Expression *GetCondition() { return condition; }
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = forToken;

        ces.str = std::format(
            "for {};{};{} {}", init ? init->GetCodeErrString().str : "",
            condition ? condition->GetCodeErrString().str : "",
            update ? update->GetCodeErrString().str : "",
            blockStatement->GetCodeErrString().str);
        return ces;
    }
    Statement *GetUpdate() { return update; }
    Statement *GetInit() { return init; }

  private:
    Token const *forToken = nullptr;
    Expression *condition = nullptr;
    Statement *init = nullptr;
    Statement *update = nullptr;
    BlockStatement *blockStatement = nullptr;
    friend class Parser;
};

class RetStatement : public Statement
{

  public:
    // virtual //std::string PrettyString(std::string &prefix) override;
    class Expression *GetExpression() const { return expression; }
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = retToken;

        ces.str = std::format(
            "ret {}", expression ? expression->GetCodeErrString().str : "");
        return ces;
    }
    class FnStatement *GetFnStatement() { return fnStmt; }

  private:
    class FnStatement *fnStmt = nullptr;
    Token const *retToken = nullptr;
    class Expression *expression = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class FnStatement : public Statement
{
  public:
    // std::string PrettyString(std::string &prefix) override;
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = fnToken;
        es.str = std::format("fn {} (", identifier->GetLexeme());
        for (VarDeclStatement *s : parameters)
        {
            es.str += std::format("{}, ", s->GetCodeErrString().str);
        }
        if (isVarArgs)
        {
            es.str += "...";
        }
        if (es.str.ends_with(','))
        {
            es.str.pop_back();
        }
        es.str += std::format(")");
        if (returnDataTypeToken)
        {
            es.str += " -> " + returnDataTypeToken->GetDataType()->GetLexeme();
        }
        return es;
    }
    Token const *GetIdentifier() const { return identifier; }
    DataTypeToken *GetReturnDatatype() { return returnDataTypeToken; }
    bool IsPrototype() { return isPrototype; }
    bool IsVarArgs() { return isVarArgs; }
    BlockStatement *GetBlockStatement() { return blockStatement; };
    std::vector<VarDeclStatement *> &GetParameters() { return parameters; }

  private:
    Token const *fnToken = nullptr;
    Token const *identifier = nullptr;
    DataTypeToken *returnDataTypeToken = nullptr;
    std::vector<VarDeclStatement *> parameters;
    BlockStatement *blockStatement = nullptr;
    bool isPrototype = false;
    bool isVarArgs = false;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class MemberAccessExpression : public Expression
{

  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str += std::format("{}.{}", leftExpr->GetCodeErrString().str,
                              rightExpr->GetCodeErrString().str);
        return es;
    }
    Token const *GetFirstToken() const override
    {
        return leftExpr->GetFirstToken();
    }

    Expression *GetLeftExpression() { return leftExpr; }
    Expression *GetRightExpression() { return rightExpr; }

  private:
    Expression *leftExpr = nullptr;
    Expression *rightExpr = nullptr;
    std::vector<Expression *> args;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class FnCallExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format("{}(", expression->GetCodeErrString().str);
        for (Expression *expr : args)
        {
            es.str += std::format("{},", expr->GetCodeErrString().str);
        }
        if (es.str.ends_with(','))
        {
            es.str.pop_back();
        }
        es.str += std::format(")");
        return es;
    }
    Token const *GetFirstToken() const override
    {
        return expression->GetFirstToken();
    }

    Expression *GetGetExpression() const { return expression; }

    std::vector<Expression *> &GetArgs() { return args; }

  private:
    class Expression *expression = nullptr;

    std::vector<Expression *> args;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class TernaryExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetTrueExpression() const { return trueExpr; }
    Expression *GetFalseExpression() const { return falseExpr; }
    Expression *GetCondition() const { return condition; }
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();
        es.str = std::format("{}? {} : {}", condition->GetCodeErrString().str,
                             trueExpr->GetCodeErrString().str,
                             falseExpr->GetCodeErrString().str);
        return es;
    }
    Token const *GetFirstToken() const override
    {
        return condition->GetFirstToken();
    }

  private:
    Expression *trueExpr = nullptr;
    Expression *falseExpr = nullptr;
    Expression *condition = nullptr;
    Token const *questionMarkToken = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class BinaryExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetLeftExpression() const { return left; }
    Expression *GetRightExpression() const { return right; }
    Token const *GetOperator() const { return op; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();
        es.str = std::format("{} {} {} ", left->GetCodeErrString().str,
                             op->GetLexeme(), right->GetCodeErrString().str);
        return es;
    }

    Token const *GetFirstToken() const override
    {
        return left->GetFirstToken();
    }

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
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetRightExpression() const { return right; }
    Token const *GetOperator() const { return op; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = postfix ? std::format("{}{}", right->GetCodeErrString().str,
                                       op->GetLexeme())
                         : std::format("{}{}", op->GetLexeme(),
                                       right->GetCodeErrString().str);
        return es;
    }

    Token const *GetFirstToken() const override
    {
        return postfix ? right->GetFirstToken() : op;
    }

    bool IsPostfix() { return postfix; }

    DataTypeToken *GetSizeOfDataTypeToken() { return sizeOfDataTypeToken; }

  private:
    Expression *right = nullptr;
    Token const *op = nullptr;
    bool postfix = false;
    DataTypeToken *sizeOfDataTypeToken = nullptr;
    //
    friend class Parser;
    friend class SemanticAnalyzer;
};

// Used on struct, arrays
class InitializerListExpression : public Expression
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = "{";

        for (Expression *expr : expressions)
        {
            es.str += expr->GetCodeErrString().str + ",";
        }
        if (es.str.ends_with(','))
        {
            es.str.pop_back();
        }
        es.str += "}";
        return es;
    }

    Token const *GetFirstToken() const override { return token; }
    std::vector<Expression *> &GetExpressions() { return expressions; }

  private:
    Token const *token = nullptr;
    std::vector<Expression *> expressions;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class SubscriptExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    Expression *GetExpression() const { return expression; }
    Expression *GetIndexExpression() const { return index; }
    Token const *GetOperator() const { return token; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format("{}[{}]", expression->GetCodeErrString().str,
                             index->GetCodeErrString().str);
        return es;
    }

    Token const *GetFirstToken() const override { return token; }

  private:
    Expression *expression = nullptr;
    Token const *token = nullptr;
    Expression *index = nullptr;
    //
    friend class Parser;
    friend class SemanticAnalyzer;
};
class GroupingExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;

    class Expression *GetExpression() const { return expression; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();
        es.str = std::format("( {} )", expression->GetCodeErrString().str);
        return es;
    }

    Token const *GetFirstToken() const override { return leftParen; }

  private:
    Expression *expression = nullptr;
    Token const *leftParen = nullptr;
    friend class Parser;
};

class CastExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;

    class Expression *GetExpression() const { return expression; }
    DataTypeToken *GetCastToType() { return castToTypeToken; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format(
            "{} as {}", expression->GetCodeErrString().str,
            castToTypeToken ? castToTypeToken->GetDataType()->GetLexeme()
                            : TokenDataTypeToString(
                                  castToTypeToken->GetDataType()->GetType()));
        return es;
    }

    Token const *GetFirstToken() const override
    {
        return expression->GetFirstToken();
    }

  private:
    Expression *expression = nullptr;
    DataTypeToken *castToTypeToken = nullptr;

    friend class Parser;
    friend class SemanticAnalyzer;
};

class PrimaryExpression : public Expression
{
  public:
    // std::string PrettyString(std::string &prefix, bool isLeft) override;
    Token const *GetPrimary() const { return primary; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format("{}", primary->GetLexeme());
        return es;
    }

    Token const *GetFirstToken() const override { return primary; }

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
    std::vector<Statement *> &Parse();
    bool IsDataType(TokenType type);

  private:
    // ParseProgram
    // ParsePackage
    Statement *ParseDeclaration();

    Statement *ParseStatement();
    Statement *ParseBlockStatement();
    Statement *ParseFnStatement();
    Statement *ParseVarDeclStatement();
    Statement *ParseVarAssignmentStatement(Expression *lValueExpr);
    Statement *ParseVarCompundAssignmentStatement(Expression *lValueExpr);
    Statement *ParseExpressionStatement();
    Statement *ParseIfStatement();
    Statement *ParseElseIfStatement();
    Statement *ParseElseStatement();
    Statement *ParseWhileLoopStatement();
    Statement *ParseForLoopStatement();
    Statement *ParseBreakStatement();
    Statement *ParseContinueStatement();
    Statement *ParseStructStatement();
    Statement *ParseRetStatement();

    Expression *ParseExpression();
    Expression *ParseLambdaExpression();
    Expression *ParseTernaryExpression();
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
    Expression *ParsePostfixExpression();
    Expression *ParseFnCallExpression();
    Expression *ParseRefExpression();
    Expression *ParseDerefExpression();
    Expression *ParseInitializerListExpression();
    Expression *ParseMemberAccessExpression();
    Expression *ParsePrimaryExpression();
    Expression *BuildBinaryExpression(Token const *op, Expression *left,
                                      Expression *right);

    DataTypeToken *ParseDataType();

    bool Consume(TokenType type);
    Token const *Peek();
    Token const *PeekAhead(size_t steps);
    Token const *Prev();
    void Advance();
    size_t currentIndex = 0;
    const std::vector<Token *> tokens;
    std::vector<Statement *> statements;
};
