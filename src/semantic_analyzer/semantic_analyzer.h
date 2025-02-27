#pragma once
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <unordered_map>
// Check if types are compatible.
//  Check if a variable is used before its declaration.
//  handeling scope.

class Variable
{
  public:
    Variable(std::string name, Token const *dataType, Token const *idToken)
        : name(name), dataType(dataType), idToken(idToken)
    {
        isSigned = ::IsSigned(dataType->GetType());
    }
    virtual ~Variable() {}
    std::string GetName() const { return name; }
    Token const *GetDataType() const { return dataType; }
    Token const *GetIdToken() const { return idToken; }
    bool IsSigned() { return isSigned; }

  protected:
    std::string name = "";
    Token const *dataType = nullptr;
    Token const *idToken = nullptr;
    bool isSigned = false;
};

class FnPointer : public Variable
{
  public:
    FnPointer(std::string name, Token const *dataType, Token const *idToken)
        : Variable(name, dataType, idToken) {};
    ~FnPointer() {};
    class FnExpression *GetFnExpression() { return fnExpr; }

  private:
    class FnExpression *fnExpr = nullptr;
    friend class SemanticAnalyzer;
};

class Scope
{
  public:
    Variable const *GetVariable(std::string name,
                                CodeErrString codeErrString) const;
    void AddVariable(Variable *variable, CodeErrString codeErrString);
    Scope(Scope *parent = nullptr) { this->parent = parent; }
    Scope const *GetParent() const { return parent; }

  private:
    Scope *parent = nullptr;
    std::unordered_map<std::string, Variable *> variablesMap;
    friend class SemanticAnalyzer;
};

class SemanticAnalyzer
{
  public:
    SemanticAnalyzer(std::vector<Statement *> &statements);
    ~SemanticAnalyzer();
    void Analize();

  private:
    void AnalizeStatement(Statement *statement);
    void PreAnalizeStatement(Statement *statement, size_t index);
    TokenType DecideDataType(Expression **left, Expression **right);
    void EvaluateAndAssignDataTypeToExpression(Expression *expression);
    Scope *currentScope = nullptr;
    std::unordered_map<BlockStatement *, Scope *> blockToScopeMap;
    std::vector<Statement *> &statements;
    BlockStatement *currentBlockStatement = nullptr;
    FnExpression *currentFunction = nullptr;
    size_t statementCurrentIndex = 0;

    friend class CodeGen;
};
