#pragma once
#include "lexer/lexer.h"
#include <cstdint>
#include <format>
#include <string>
#include <vector>

struct CodeErrString
{
    Token const *firstToken;
    std::string str;
};

struct Generic
{
    std::vector<class DataTypeToken *> types;
};
struct Tokens
{
    size_t startPos;
    size_t endPos;
    std::vector<Token *> tokens;
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

    Generic *generic = nullptr;
    Tokens tokens;
    const Token *libToken = nullptr;
    const Token *pkgToken = nullptr;

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
    virtual CodeErrString GetCodeErrString() = 0;
};
class Package
{
  public:
    const Token *GetIdentifier() const { return identifier; }
    std::vector<Statement *> &GetStatements() { return statements; }

  private:
    const Token *identifier = nullptr;
    std::vector<Statement *> statements;

    friend class Parser;
    friend class SemanticAnalyzer;
};

class ImportStatement : public Statement
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = token;
        es.str += std::format("import {}", package->GetCodeErrString().str);
        return es;
    }

    Expression *GetPackageExpression() { return package; }

  private:
    Expression *package = nullptr;
    const Token *token = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class DeferStatement : public Statement
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = token;
        es.str = token->GetLexeme();
        return es;
    }
    Statement *GetStatement() { return statement; }

  private:
    bool moved = false;
    Statement *statement;
    const Token *token = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};
class EmptyStatement : public Statement
{

  public:
    ~EmptyStatement() {}
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

    bool IsPublic() { return (bool)pub; };

  private:
    Token const *pub = nullptr;
    Token const *identifier = nullptr;
    DataTypeToken *dataTypeToken = nullptr;
    // datatype
    Expression *expression = nullptr;

    bool isParamter = false;
    bool isField = false;
    bool isGlobal = false;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};
class VarCompoundAssignmentStatement : public Statement
{
  public:
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
        ces.str = std::format("struct {}\n",
                              identifier ? identifier->GetLexeme() : "");
        // ces.str += "{\n";
        // for (auto field : fields)
        // {
        //     ces.str += field->GetCodeErrString().str;
        //     ces.str += ";\n";
        // }

        // ces.str += "}";

        return ces;
    }
    const std::vector<VarDeclStatement *> &GetFields() { return fields; }
    const std::vector<FnStatement *> &GetMethods() { return methods; }

    bool IsPublic() { return (bool)pub; };

    Token const *GetIDToken() { return identifier; }

  private:
    Token const *pub = nullptr;

    Token const *token = nullptr;
    Token const *identifier = nullptr;
    std::vector<VarDeclStatement *> fields;
    std::vector<FnStatement *> methods;
    std::vector<class UnionStatement *> unions;
    std::vector<Statement *> fieldsInOrder;
    Generic *generic = nullptr;
    Tokens tokens;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class CodeGen;
    friend class Scope;
};
class UnionStatement : public Statement
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str =
            std::format("{}\n", identifier ? identifier->GetLexeme() : "");
        // ces.str += "{\n";
        // for (auto field : fields)
        // {
        //     ces.str += field->GetCodeErrString().str;
        //     ces.str += ";\n";
        // }

        // ces.str += "}";

        return ces;
    }
    const std::vector<VarDeclStatement *> &GetFields() { return fields; }

    bool IsPublic() { return (bool)pub; };

  private:
    Token const *pub = nullptr;

    Token const *token = nullptr;
    Token const *identifier = nullptr;
    std::vector<class VarDeclStatement *> fields;
    std::vector<class StructStatement *> structs;
    std::vector<class Statement *> fieldsInOrder;
    Generic *generic = nullptr;
    Tokens tokens;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class Scope;
};

class EnumStatement : public Statement
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str = std::format(" {} : {}\n", identifier->GetLexeme(),
                              datatype->ToString());
        // ces.str += "{\n";
        // for (auto field : fields)
        // {
        //     ces.str += field->identifier->GetLexeme();
        //     ces.str += ces.str +=
        //         field->expr ? "=" + field->expr->GetCodeErrString().str : "";
        //     ces.str += ",\n";
        // }

        // ces.str += "}";

        return ces;
    }

    struct Field
    {
        Token const *identifier = nullptr;
        Expression *expr = nullptr;
        uint64_t uValue = 0;
        int64_t sValue = 0;
        bool useSigned = false;
    };

    bool IsPublic() { return (bool)pub; };

  private:
    Token const *pub = nullptr;
    Token const *token = nullptr;
    Token const *identifier = nullptr;
    DataTypeToken *datatype = nullptr;
    std::vector<Field *> fields;

    friend class Parser;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class ExpressionStatement : public Statement
{

  public:
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
    std::vector<Statement *> &GetStatements() { return statements; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces{};
        ces.firstToken = statements.size() > 0
                             ? statements[0]->GetCodeErrString().firstToken
                             : nullptr;
        // ces.str += "{\n";
        // for (Statement *s : statements)
        // {
        //     ces.str += std::format("    {}\n", s->GetCodeErrString().str);
        // }
        // ces.str += "}\n";
        return ces;
    }
    size_t index = 0;

  private:
    bool loopBlock = false;
    std::vector<Statement *> statements;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class SwitchStatement : public Statement
{

  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = token;
        ces.str += "switch " + matchExpr->GetCodeErrString().str + "\n{\n";
        // for (auto c : cases)
        // {
        //     ces.str += "case ";
        //     for (auto expr : c->exprs)
        //     {
        //         ces.str += expr->GetCodeErrString().str + ",";
        //     }
        //     if (ces.str.ends_with(','))
        //     {
        //         ces.str.pop_back();
        //     }
        //     ces.str += ":\n";
        //     ces.str += c->blockStatement->GetCodeErrString().str;
        //     ces.str += "\n";
        // }

        // if (defualtCase)
        // {
        //     ces.str += "defualt:\n";
        //     ces.str += defualtCase->blockStatement->GetCodeErrString().str;
        // }
        // ces.str += "\n}";
        return ces;
    }

    Expression *const GetMatchExpr() const { return matchExpr; };
    struct Case
    {
        std::vector<Expression *> exprs;
        BlockStatement *blockStatement;
    };
    const std::vector<Case *> GetCases() { return cases; };
    Case *const GetDefualtCase() { return defualtCase; }

  private:
    const Token *token = nullptr;
    Expression *matchExpr = nullptr;

    std::vector<Case *> cases;
    Case *defualtCase = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class IfStatement : public Statement
{
  public:
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
    friend class SemanticAnalyzer;
};

class ElseIfStatement : public Statement
{
  public:
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
    friend class SemanticAnalyzer;
};
class ElseStatement : public Statement
{
  public:
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
    friend class SemanticAnalyzer;
};

class WhileLoopStatement : public Statement
{
  public:
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
    class Expression *GetExpression() const { return expression; }
    CodeErrString GetCodeErrString() override
    {
        CodeErrString ces = {};
        ces.firstToken = retToken;

        ces.str = std::format(
            "ret {}", expression ? expression->GetCodeErrString().str : "");
        return ces;
    }

  private:
    Token const *retToken = nullptr;
    class Expression *expression = nullptr;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class FnStatement : public Statement
{
  public:
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

    bool IsPublic() { return (bool)pub; };
    Generic *GetGeneric() { return generic; }

  private:
    Token const *pub = nullptr;

    Token const *fnToken = nullptr;
    Token const *identifier = nullptr;
    DataTypeToken *returnDataTypeToken = nullptr;
    std::vector<VarDeclStatement *> parameters;
    BlockStatement *blockStatement = nullptr;
    bool isPrototype = false;
    bool isVarArgs = false;

    struct Method
    {
        Token const *selfToken = nullptr;
        Token const *readonly = nullptr;
        Token const *asterisk = nullptr;
    };

    Method *method = nullptr;

    class StructStatement *structStmt = nullptr;
    Generic *generic = nullptr;
    Tokens tokens;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class Scope;
};

class FnExpression : public Expression
{
  public:
    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = fnToken;
        es.str = std::format("fn (");
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
    DataTypeToken *GetReturnDatatype() { return returnDataTypeToken; }
    bool IsPrototype() { return isPrototype; }
    bool IsVarArgs() { return isVarArgs; }
    BlockStatement *GetBlockStatement() { return blockStatement; };
    std::vector<VarDeclStatement *> &GetParameters() { return parameters; }
    Token const *GetFirstToken() const override { return fnToken; }
    std::string GetName() { return name; };

  private:
    std::string name;
    Token const *fnToken = nullptr;
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

    enum class AccessType
    {
        UNKNOWN,
        STRUCT,
        UNION,
        ENUM,
        PACKAGE,
        LIBRARY,
    };

    AccessType accessType = AccessType::UNKNOWN;

  private:
    Expression *leftExpr = nullptr;
    Expression *rightExpr = nullptr;
    const Token *token = nullptr;
    std::vector<Expression *> args;
    friend class Parser;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class FnCallExpression : public Expression
{
  public:
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
    Generic *generic = nullptr;
    std::vector<Expression *> args;
    friend class Parser;
    friend class SemanticAnalyzer;
};

class TernaryExpression : public Expression
{
  public:
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

        // es.str = "{";

        // for (Expression *expr : expressions)
        // {
        //     if (expr)
        //         es.str += expr->GetCodeErrString().str + ",";
        //     else
        //         es.str += "null,";
        // }
        // if (es.str.ends_with(','))
        // {
        //     es.str.pop_back();
        // }
        // es.str += "}";
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
    class Expression *GetExpression() const { return expression; }
    DataTypeToken *GetCastToType() { return castToTypeToken; }

    CodeErrString GetCodeErrString() override
    {
        CodeErrString es = {};
        es.firstToken = GetFirstToken();

        es.str = std::format(
            "{} as {}", expression->GetCodeErrString().str,
            castToTypeToken ? castToTypeToken->GetDataType()->GetLexeme() : "");
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
    friend class CodeGen;
};

class Parser
{
  public:
    Parser(std::vector<Token *> &tokens);
    ~Parser();
    void PrettyPrintAST();
    std::vector<Package *> &Parse();
    bool IsDataType(TokenType type);

  private:
    // ParseProgram
    // ParsePackage

    Package *ParsePackage();

    Statement *ParseDeclaration();

    Statement *ParseStatement();
    Statement *ParseBlockStatement();
    Statement *ParseFnStatement(bool isMethod = false);
    Statement *ParseVarDeclStatement();
    Statement *ParseVarAssignmentStatement(Expression *lValueExpr);
    Statement *ParseVarCompundAssignmentStatement(Expression *lValueExpr);
    Statement *ParseExpressionStatement();
    Statement *ParseIfStatement();
    Statement *ParseElseIfStatement();
    Statement *ParseElseStatement();
    Statement *ParseSwitchStatement();
    Statement *ParseWhileLoopStatement();
    Statement *ParseForLoopStatement();
    Statement *ParseBreakStatement();
    Statement *ParseContinueStatement();
    Statement *ParseStructStatement(bool anonymous = false);
    Statement *ParseUnionStatement();
    Statement *ParseEnumStatement();
    Statement *ParseRetStatement();
    Statement *ParseDeferStatement();
    Statement *ParseImportStatement();

    Expression *ParseExpression();
    Expression *ParseFnExpression();
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

    DataTypeToken *ParseDataType(bool check = false);

    void AddSelfParam(FnStatement *method, const Token *dataType);
    Generic *ParseGeneric();
    bool Consume(TokenType type);
    Token const *Peek();
    Token const *PeekAhead(size_t steps);
    Token const *Prev();
    void Advance();
    size_t currentIndex = 0;
    bool isAnonymous = false;
    std::vector<Token *> tokens;
    std::vector<Package *> packages;

    friend class Scope;
    friend class SemanticAnalyzer;
};
