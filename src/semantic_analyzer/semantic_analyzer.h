#pragma once
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <unordered_map>
#include <vector>
class DataType
{
  public:
    enum class Type
    {
        UNKNOWN,
        NOTYPE,
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
        F32,
        F64,
        BOOL,
        STRING,
        STRUCT,
        ENUM,
        UNION,
        FNPOINTER,
        POINTER,
    };
    Type GetType() { return type; }

    std::string ToString();
    bool IsNumerical();
    bool IsInteger();
    bool IsFloat();
    bool IsSigned();
    TokenType ToTokenType();

  protected:
    virtual std::string AggregateTypeToString() { return ""; }
    Type type;
    size_t typeWidth = 0;
    size_t alignment = 4;
    Token const *dataTypeToken;

    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

bool IsPrimaryDataType(TokenType type);
class StructDataType : public DataType
{
  public:
    virtual std::string AggregateTypeToString() override { return name; }

  private:
    std::string name = "";
    std::vector<class Variable *> fields;
};
class UnionDataType : public DataType
{
  public:
    virtual std::string AggregateTypeToString() override { return name; }

  private:
    std::string name = "";
    std::vector<class Variable *> fields;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};
template <typename T>

class EnumDataType : public DataType
{

  public:
    virtual std::string AggregateTypeToString() override { return name; }

  private:
    std::string name = "";
    struct Field
    {
        PrimaryExpression *identifier;
        T value;
    };
    std::vector<Field> fields;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

std::string FnPointerDataTypeToStringKey(class FnPointerDataType *fnDataType);

class FnPointerDataType : public DataType
{
  public:
    std::vector<DataType *> GetParameters() { return paramters; }
    DataType *GetReturnDataType() { return returnDataType; }

  private:
    DataType *returnDataType;
    std::vector<DataType *> paramters;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class PointerDataType : public DataType
{
  public:
    PointerDataType() {}
    DataType *PointedToDatatype() { return dataType; }

  private:
    // depends on host arch.
    DataType *dataType;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class ArrayDataType : public PointerDataType
{
  public:
  private:
    size_t size = 0;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class StringDataType : public PointerDataType
{
  public:
    StringDataType() { type = DataType::Type::STRING; }

  private:
    size_t len;
    size_t sizeInBytes;
    friend class Scope;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};
class Variable
{
  public:
    Variable(std::string name) : name(name) {}
    virtual ~Variable() {}
    std::string GetName() const { return name; }
    Token const *GetToken() { return token; }

  protected:
    Token const *token = nullptr;
    std::string name = "";
    DataType *dataType = nullptr;

    friend class SemanticAnalyzer;
};

class Function
{
  public:
    std::string GetName() { return name; }
    class FnStatement *GetFnStatement() { return fnStmt; }

  private:
    std::string name;
    class FnStatement *fnStmt = nullptr;
    FnPointerDataType *fnPointer = nullptr;
    friend class SemanticAnalyzer;
};

class Scope
{
  public:
    Variable *GetVariable(std::string name, CodeErrString codeErrString);
    void AddVariable(Variable *variable, CodeErrString codeErrString);

    void AddFunction(Function *function, CodeErrString codeErrString);
    Function *GetFunction(std::string name, CodeErrString codeErrString);
    Scope(Scope *parent = nullptr);

    Scope const *GetParent() const { return parent; }

  private:
    Scope *parent = nullptr;
    BlockStatement *currentBlockStatement = nullptr;
    std::unordered_map<std::string, Variable *> variablesMap;
    std::unordered_map<std::string, Function *> functionsMap;
    std::unordered_map<std::string, DataType *> datatypesMap;
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
    DataType::Type DecideDataType(Expression **left, Expression **right);
    void EvaluateAndAssignDataTypeToExpression(Expression *expression);
    Scope *currentScope = nullptr;
    std::unordered_map<BlockStatement *, Scope *> blockToScopeMap;
    std::unordered_map<Expression *, DataType *> exprToDataTypeMap;
    std::unordered_map<Statement *, DataType *> stmtToDataTypeMap;
    std::vector<Statement *> &statements;
    Function *currentFunction = nullptr;
    size_t statementCurrentIndex = 0;
    friend class CodeGen;
};
