#pragma once
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <unordered_map>
// Check if types are compatible.
//  Check if a variable is used before its declaration.
//  handeling scope.

bool AreTypesCompatible(TokenType left, TokenType right);
class Variable
{
  public:
    Variable(std::string name, Token const *dataType, Token const *idToken)
        : name(name), dataType(dataType), idToken(idToken)
    {
        isSigned = ::IsSigned(dataType->GetType());
    }
    ~Variable() {}
    std::string GetName() const { return name; }
    Token const *GetDataType() const { return dataType; }
    Token const *GetIdToken() const { return idToken; }
    bool IsSigned() { return isSigned; }

  private:
    std::string name = "";
    Token const *dataType = nullptr;
    Token const *idToken = nullptr;
    bool isSigned = false;
};

class Scope
{
  public:
    Variable const *GetVariable(std::string name,
                                Token const *tokenForErrorHandeling = nullptr);
    void AddVariable(Variable *variable);

  private:
    Scope *enclosing = nullptr;
    std::unordered_map<std::string, Variable *> variablesMap;
    friend class SemanticAnalyzer;
};

class SemanticAnalyzer
{
  public:
    SemanticAnalyzer(const std::vector<Statement *> &statements);
    ~SemanticAnalyzer();
    void Analize();

  private:
    TokenType DecideDataType(Expression **left, Expression **right);
    void EvaluateAndAssignDataTypeToExpression(Expression *expression);
    Scope globalScope;
    Scope *currentScope = nullptr;
    const std::vector<Statement *> statements;
};
