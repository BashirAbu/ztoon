#pragma once
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <unordered_map>
#include <vector>
class DataType
{
  public:
    virtual ~DataType() {}

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
        STRUCT,
        ENUM,
        UNION,
        FN,
        POINTER,
        ARRAY,
        InitList,
    };
    Type GetType() { return type; }

    std::string ToString();
    bool IsNumerical();
    bool IsInteger();
    bool IsFloat();
    bool IsSigned();
    bool IsReadOnly() { return isReadOnly; }
    TokenType ToTokenType();

    Type type;
    bool isReadOnly = false;
    bool complete = true;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class InitListType : public DataType
{

  public:
    InitListType() {}
    std::vector<DataType *> &Datatype() { return dataTypes; }

    std::vector<DataType *> dataTypes;
    bool allSameType = false;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};
bool IsPrimaryDataType(TokenType type);
class StructDataType : public DataType
{
  public:
    std::string name = "";
    Scope *scope = nullptr;
    StructStatement *structStmt;
    std::vector<DataType *> fields;
};
class UnionDataType : public DataType
{
  public:
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

class ArrayDataType : public DataType
{
  public:
    ArrayDataType() {}
    DataType *dataType = nullptr;
    size_t size = 0;
    Expression *sizeExpr = nullptr;
};
class PointerDataType : public DataType
{
  public:
    PointerDataType() {}
    DataType *PointedToDatatype() { return dataType; }

    DataType *dataType;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class FnDataType : public DataType
{
  public:
    std::vector<DataType *> GetParameters() { return paramters; }
    DataType *GetReturnDataType() { return returnDataType; }
    bool IsVarArgs() { return isVarArgs; }
    PointerDataType *GetFnPtrType()
    {
        PointerDataType *fnPtr = gZtoonArena.Allocate<PointerDataType>();
        fnPtr->type = DataType::Type::POINTER;
        fnPtr->dataType = this;
        return fnPtr;
    }

  private:
    DataType *returnDataType;
    std::vector<DataType *> paramters;
    bool isVarArgs = false;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};
class Symbol
{
  public:
    virtual ~Symbol() {}
    virtual std::string GetName() = 0;
    virtual DataType *GetDataType() = 0;

  private:
};

class Variable : public Symbol
{
  public:
    Variable(std::string name) : name(name) {}
    virtual ~Variable() {}
    std::string GetName() override { return name; }
    Token const *GetToken() { return token; }
    DataType *GetDataType() override { return dataType; }

  protected:
    Token const *token = nullptr;
    std::string name = "";
    DataType *dataType = nullptr;

    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class Function : public Symbol
{
  public:
    std::string GetName() override { return name; }
    class FnStatement *GetFnStatement() { return fnStmt; }
    void AddParamter(Variable *var, CodeErrString codeErrString);

    DataType *GetDataType() override { return fnPointer; }
    FnDataType *GetFnDataTypeFromFnPTR()
    {
        return dynamic_cast<FnDataType *>(fnPointer->dataType);
    }

  private:
    std::string name;
    class FnStatement *fnStmt = nullptr;
    class RetStatement *retStmt = nullptr;
    PointerDataType *fnPointer = nullptr;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class Scope
{
  public:
    Scope(class SemanticAnalyzer *semanticAnalyzer, Scope *parent = nullptr);
    Symbol *GetSymbol(std::string name, CodeErrString codeErrString);
    void AddSymbol(Symbol *symbol, CodeErrString codeErrString);

    Scope const *GetParent() const { return parent; }

    DataType *GetDataType(DataTypeToken *dataTypeToken);

  private:
    Scope *parent = nullptr;
    bool lookUpParent = true;
    std::unordered_map<std::string, Symbol *> symbolsMap;
    std::unordered_map<std::string, DataType *> datatypesMap;
    class SemanticAnalyzer *semanticAnalyzer;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class SemanticAnalyzer
{
  public:
    SemanticAnalyzer(std::vector<Statement *> &statements);
    ~SemanticAnalyzer();
    void Analize();

  private:
    void AnalizeStatement(Statement *statement);

    void ValidateAssignValueToVarArray(Expression *expr,
                                       ArrayDataType *arrType);

    void ValidateAssignValueToVarStruct(Expression *expr,
                                        StructDataType *arrType);
    void PreAnalizeStatement(Statement *statement, size_t index);
    DataType::Type DecideDataType(Expression **left, Expression **right);
    void EvaluateAndAssignDataTypeToExpression(Expression *expression);
    Scope *currentScope = nullptr;
    std::unordered_map<BlockStatement *, Scope *> blockToScopeMap;
    std::unordered_map<Expression *, DataType *> exprToDataTypeMap;
    std::unordered_map<Statement *, DataType *> stmtToDataTypeMap;
    std::vector<Statement *> &statements;
    BlockStatement *currentBlockStatement = nullptr;
    size_t statementCurrentIndex = 0;
    Function *currentFunction = nullptr;
    size_t inLoop = 0;
    friend class CodeGen;
    friend class Scope;
};
