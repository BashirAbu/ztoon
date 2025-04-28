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
        PACKAGE,
        LIBRARY,
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
class Symbol
{
  public:
    virtual ~Symbol() {}
    virtual std::string GetName() = 0;
    virtual DataType *GetDataType() = 0;
    bool IsPublic() { return isPublic; }

  protected:
    bool isPublic = false;
    friend class SemanticAnalyzer;
};

class PackageDataType : public DataType, public Symbol
{
  public:
    virtual std::string GetName() override { return name; }
    virtual DataType *GetDataType() override { return this; }
    Package *pkg = nullptr;
    std::string name;
    friend class SemanticAnalyzer;
};

class LibraryDataType : public DataType, public Symbol
{
  public:
    virtual std::string GetName() override { return name; }
    virtual DataType *GetDataType() override { return this; }
    class Library *lib = nullptr;
    std::string name;
    friend class SemanticAnalyzer;
};
class StructDataType : public DataType, public Symbol
{
  public:
    virtual std::string GetName() override { return name; }
    std::string GetFullName() { return fullName; }
    std::string fullName = "";
    virtual DataType *GetDataType() override { return this; }
    std::string name = "";
    Scope *scope = nullptr;
    StructStatement *structStmt;
    InitializerListExpression *defaultValuesList = nullptr;
    std::vector<DataType *> fields;
    friend class SemanticAnalyzer;
};
class UnionDataType : public DataType, public Symbol
{
  public:
    virtual std::string GetName() override { return name; }
    std::string GetFullName() { return fullName; }
    std::string fullName = "";
    virtual DataType *GetDataType() override { return this; }
    DataType *largestDatatype = nullptr;
    std::string name = "";
    Scope *scope = nullptr;
    UnionStatement *unionStmt;
    std::vector<DataType *> fields;
    friend class SemanticAnalyzer;
};

class EnumDataType : public DataType, public Symbol
{
  public:
    virtual std::string GetName() override { return name; }
    std::string GetFullName() { return fullName; }
    std::string fullName = "";
    virtual DataType *GetDataType() override { return this; }
    std::string name;
    EnumStatement *enumStmt = nullptr;
    DataType *datatype = nullptr;
    Scope *scope = nullptr;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
    friend class SemanticAnalyzer;
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
    bool isNullPtr = false;
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
    bool IsMethod() { return isMethod; }

  private:
    DataType *returnDataType;
    std::vector<DataType *> paramters;
    bool isVarArgs = false;
    bool isMethod = false;
    friend class Scope;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class ReadonlySymbol : public Symbol
{
  public:
    ReadonlySymbol(std::string name) : name(name) {}
    virtual ~ReadonlySymbol() {}
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
    VarDeclStatement *varDeclStmt = nullptr;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class Function : public Symbol
{
  public:
    std::string GetName() override { return name; }
    // class FnStatement *GetFnStatement() { return fnStmt; }
    void AddParamter(Variable *var, CodeErrString codeErrString);

    DataType *GetDataType() override { return fnPointer; }
    FnDataType *GetFnDataTypeFromFnPTR()
    {
        return dynamic_cast<FnDataType *>(fnPointer->dataType);
    }

  private:
    std::string name;
    class RetStatement *retStmt = nullptr;
    PointerDataType *fnPointer = nullptr;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

class Scope
{
  public:
    Scope(class SemanticAnalyzer *semanticAnalyzer, std::string name,
          Scope *parent = nullptr);
    Symbol *GetSymbol(std::string name, CodeErrString codeErrString,
                      bool check = false);
    void AddSymbol(Symbol *symbol, CodeErrString codeErrString);

    Scope const *GetParent() const { return parent; }

    DataType *GetDataType(DataTypeToken *dataTypeToken);

  private:
    std::string name;
    std::vector<Scope *> importedPackages;
    std::vector<Library *> importedLibs;
    Scope *parent = nullptr;
    bool lookUpParent = true;
    std::unordered_map<std::string, Symbol *> symbolsMap;
    std::unordered_map<std::string, DataType *> datatypesMap;
    class SemanticAnalyzer *semanticAnalyzer;
    friend class SemanticAnalyzer;
    friend class CodeGen;
};

struct Library
{
    std::string name;
    std::vector<Package *> packages;
    std::vector<Library *> libs;
    bool analyed = false;
};

struct TopVarDecl
{
    VarDeclStatement *varDecl = nullptr;
    Scope *currentScope = nullptr;
};
struct TopAggTypeDecl
{
    StructStatement *structStmt = nullptr;
    UnionStatement *unionStmt = nullptr;
    EnumStatement *enumStmt = nullptr;
    Scope *currentScope = nullptr;
};

class SemanticAnalyzer
{
  public:
    SemanticAnalyzer(std::vector<Package *> _packages,
                     std::vector<Library *> _libraries);
    ~SemanticAnalyzer();

  private:
    void Analyze(std::vector<Package *> &packages);
    void AnalyzePackage(Package *pkg);
    void AnalyzePackageGlobalTypes(Package *pkg);
    void AnalyzePackageGlobalFuncsAndVars(Package *pkg);

    void AnalyzePackageGlobalTypeBodies(Package *pkg);
    void AnalyzePackageVarAndFuncBodies(Package *pkg);
    void AnalyzeStatement(Statement *statement);
    void ValidateAssignValueToVarArray(Expression *expr,
                                       ArrayDataType *arrType);
    void ValidateAssignValueToVarStruct(Expression *expr,
                                        StructDataType *arrType);
    void PreAnalyzeStatement(Statement *statement, size_t index);

    void AnalyzeBlockStatement(BlockStatement *blockStmt);
    void AnalyzeFnStatement(FnStatement *fnStmt, bool isGlobal,
                            bool analyzeSymbol, bool analyzeBody,
                            bool isMethod = false);
    void AnalyzeVarDeclStatement(VarDeclStatement *varDeclStmt, bool isGlobal,
                                 bool analyzeSymbol, bool analyzeBody);
    void
    AnalyzeVarAssignmentStatement(VarAssignmentStatement *varAssignmentStmt);
    void AnalyzeVarCompundAssignmentStatement(
        VarCompoundAssignmentStatement *varComStmt);
    void AnalyzeExpressionStatement(ExpressionStatement *exprStmt);
    void AnalyzeIfStatement(IfStatement *ifStmt);
    void AnalyzeElseIfStatement(ElseIfStatement *elifStmt);
    void AnalyzeElseStatement(ElseStatement *elseStmt);
    void AnalyzeSwitchStatement(SwitchStatement *switchStmt);
    void AnalyzeWhileLoopStatement(WhileLoopStatement *whileLoopStmt);
    void AnalyzeForLoopStatement(ForLoopStatement *forLoopStmt);
    void AnalyzeBreakStatement(BreakStatement *breakStmt);
    void AnalyzeContinueStatement(ContinueStatement *continueStmt);
    void AnalyzeStructStatement(StructStatement *structStmt, bool isGlobal,
                                bool analyzeSymbol, bool analyzeBody,
                                bool analyzeMethodsBody);
    void AnalyzeUnionStatement(UnionStatement *unionStmt, bool isGlobal,
                               bool analyzeSymbol, bool analyzeBody);
    void AnalyzeEnumStatement(EnumStatement *enumStmt, bool isGlobal,
                              bool analyzeSymbol, bool analyzeBody);
    void AnalyzeRetStatement(RetStatement *retStmt);
    void AnalyzeImportStatement(ImportStatement *importStmt);

    void AnalyzeFnExpression(FnExpression *fnExpression);
    void AnalyzeTernaryExpression(TernaryExpression *ternaryExpr);
    void AnalyzeBinaryExpression(BinaryExpression *binaryExpr);
    void AnalyzeCastExpression(CastExpression *castExpr);
    void AnalyzeGroupingExpression(GroupingExpression *groupingEpxr);
    void AnalyzeSubScriptExpression(SubscriptExpression *subExpr);
    void AnalyzeUnaryExpression(UnaryExpression *unaryExpr);
    void AnalyzeFnCallExpression(FnCallExpression *fnCallExpr);
    void
    AnalyzeInitializerListExpression(InitializerListExpression *initListEpxr);
    void AnalyzeMemberAccessExpression(MemberAccessExpression *maExpr);
    void AnalyzePrimaryExpression(PrimaryExpression *primaryExpr);

    DataType::Type DecideDataType(Expression **left, Expression **right);
    void AnalyzeExpression(Expression *expression);
    Scope *currentScope = nullptr;
    std::unordered_map<Library *, LibraryDataType *> libToDataTypeMap;
    std::unordered_map<Package *, PackageDataType *> pkgToDataTypeMap;
    std::unordered_map<Package *, Scope *> pkgToScopeMap;
    std::unordered_map<BlockStatement *, Scope *> blockToScopeMap;
    std::unordered_map<Expression *, DataType *> exprToDataTypeMap;
    std::unordered_map<Statement *, DataType *> stmtToDataTypeMap;
    std::unordered_map<Expression *, Expression *> methodToCallerMap;
    std::unordered_map<Function *, std::vector<TopVarDecl>> fnToVarDeclsMap;
    std::unordered_map<Function *, std::vector<TopAggTypeDecl>> fnToAggDeclsMap;
    std::vector<Package *> packages;
    std::vector<Library *> libraries;
    Package *currentPackage = nullptr;
    Library *currentLibrary = nullptr;
    BlockStatement *currentBlockStatement = nullptr;
    size_t statementCurrentIndex = 0;
    Function *currentFunction = nullptr;
    size_t inLoop = 0;
    friend class CodeGen;
    friend class Scope;
};
