#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include <algorithm>
#include <cstring>
#include <format>
#include <functional>

std::string DataType::ToString()
{
    std::string str;
    if (isReadOnly)
    {
        str += "readonly ";
    }
    switch (type)
    {
    case DataType::Type::I8:
    {

        str += "i8";
        break;
    }
    case DataType::Type::I16:
    {
        str += "i16";
        break;
    }
    case DataType::Type::I32:
    {
        str += "i32";
        break;
    }
    case DataType::Type::I64:
    {
        str += "i64";
        break;
    }
    case DataType::Type::U8:
    {
        str += "u8";
        break;
    }
    case DataType::Type::U16:
    {
        str += "u16";
        break;
    }
    case DataType::Type::U32:
    {
        str += "u32";
        break;
    }
    case DataType::Type::U64:
    {
        str += "u64";
        break;
    }
    case DataType::Type::F32:
    {
        str += "f32";
        break;
    }
    case DataType::Type::F64:
    {
        str += "f64";
        break;
    }
    case DataType::Type::BOOL:
    {
        str += "bool";
        break;
    }
    case DataType::Type::NOTYPE:
    {
        str += "notype";
        break;
    }
    case DataType::Type::STRUCT:
    {
        auto structType = dynamic_cast<StructDataType *>(this);
        str += structType->name;
        break;
    }
    case DataType::Type::ENUM:
    {
        auto enumType = dynamic_cast<EnumDataType *>(this);
        str += enumType->name;
        break;
    }
    case DataType::Type::UNION:
    {
        auto unionType = dynamic_cast<UnionDataType *>(this);
        str += unionType->name;
        break;
    }
    case DataType::Type::POINTER:
    {
        auto ptrType = (PointerDataType *)this;
        str += ptrType->dataType->ToString();
        auto fnType = dynamic_cast<FnDataType *>(ptrType->dataType);
        str += fnType ? "" : "*";
        break;
    }
    case DataType::Type::ARRAY:
    {
        auto arrType = (ArrayDataType *)this;
        str += arrType->dataType->ToString();
        if (currentStage == Stage::CODE_GEN)
        {
            str += std::format("[{}]", arrType->size);
        }
        else
        {
            str += "[]";
        }
    }
    break;
    case DataType::Type::InitList:
    {
        auto listType = dynamic_cast<InitListType *>(this);
        str += "{";
        for (auto t : listType->dataTypes)
        {
            str += t->ToString() + ",";
        }
        if (str.ends_with(','))
        {
            str.pop_back();
        }
        str += "}";
    }
    break;
    case DataType::Type::FN:
    {
        auto fnPtr = dynamic_cast<FnDataType *>(this);
        str += "(fn";
        str += "(";
        for (auto t : fnPtr->GetParameters())
        {
            str += t->ToString();
            str += ",";
        }
        if (fnPtr->IsVarArgs())
        {
            str += "...";
        }
        if (str.ends_with(','))
        {
            str.pop_back();
        }
        str += ")";
        if (fnPtr->GetReturnDataType())
        {
            str += "->";
            str += fnPtr->GetReturnDataType()->ToString();
        }
        str += ")";
    }
    break;
    case DataType::Type::PACKAGE:
    {
        auto pkg = dynamic_cast<PackageDataType *>(this);
        str += pkg->GetName();
    }
    break;
        break;
    default:
    {
        str += "Unknown type";
        break;
    }
    }

    return str;
}

DataType *Scope::GetDataType(DataTypeToken *dataTypeToken)
{
    std::string typeStr = dataTypeToken->ToString();

    DataType *result = nullptr;
    if (typeStr.empty())
    {
        typeStr = "notype";
    }

    Scope *currentScope = this;
    if (dataTypeToken->pkgToken)
    {
        Package *pkgFound = nullptr;
        for (auto pkg : semanticAnalyzer->packages)
        {
            if (pkg->GetIdentifier()->GetLexeme() ==
                dataTypeToken->pkgToken->GetLexeme())
            {
                pkgFound = pkg;
                break;
            }
        }
        if (!pkgFound)
        {
            CodeErrString ces;
            ces.firstToken = dataTypeToken->pkgToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("Package '{}' is not found",
                                    dataTypeToken->pkgToken->GetLexeme()),
                        ces);
        }
        currentScope = semanticAnalyzer->pkgToScopeMap[pkgFound];
    }
    if (!dataTypeToken->arrayDesc)
    {
        while (currentScope)
        {
            if (currentScope->datatypesMap.contains(typeStr))
            {
                DataType *type = currentScope->datatypesMap[typeStr];
                return type;
            }
            for (auto pkg : currentScope->importedPackages)
            {
                if (pkg->datatypesMap.contains(typeStr))
                {
                    DataType *type = pkg->datatypesMap[typeStr];
                    return type;
                }
            }
            currentScope = currentScope->parent;
        }
    }

    if (dataTypeToken->fnStatement)
    {
        auto fnType = gZtoonArena.Allocate<FnDataType>();
        fnType->type = DataType::Type::FN;

        for (auto param : dataTypeToken->fnStatement->GetParameters())
        {
            DataType *pType = GetDataType(param->GetDataType());
            if (!pType)
            {
                ReportError(std::format("Unkown type '{}'",
                                        param->GetDataType()->ToString()),
                            param->GetCodeErrString());
            }
            fnType->paramters.push_back(pType);
        }

        if (dataTypeToken->fnStatement->GetReturnDatatype())
        {

            DataType *retType =
                GetDataType(dataTypeToken->fnStatement->GetReturnDatatype());
            if (!retType)
            {
                ReportError(
                    std::format("Unkown type '{}'",
                                dataTypeToken->fnStatement->GetReturnDatatype()
                                    ->ToString()),
                    dataTypeToken->fnStatement->GetReturnDatatype()
                        ->GetCodeErrString());
            }
            fnType->returnDataType = retType;
        }
        fnType->isVarArgs = dataTypeToken->fnStatement->IsVarArgs();
        auto fnPtrType = fnType->GetFnPtrType();
        datatypesMap[fnPtrType->ToString()];
        return fnPtrType;
    }

    if (dataTypeToken->arrayDesc)
    {
        auto arrType = gZtoonArena.Allocate<ArrayDataType>();
        arrType->type = DataType::Type::ARRAY;

        arrType->sizeExpr = dataTypeToken->arrayDesc->arraySizeExpr;
        if (arrType->sizeExpr)
        {
            semanticAnalyzer->AnalyzeExpression(arrType->sizeExpr);
        }
        arrType->isReadOnly = dataTypeToken->readOnly;
        dataTypeToken->arrayDesc->dataTypeToken->readOnly = nullptr;
        arrType->dataType =
            GetDataType(dataTypeToken->arrayDesc->dataTypeToken);
        if (!arrType->dataType->complete)
        {
            ReportError(
                "Array type has incomplete element type",
                dataTypeToken->arrayDesc->dataTypeToken->GetCodeErrString());
        }
        result = arrType;
    }
    if (dataTypeToken->pointerDesc)
    {

        PointerDataType *ptrDataType = gZtoonArena.Allocate<PointerDataType>();

        ptrDataType->type = DataType::Type::POINTER;
        if (dataTypeToken->readOnly)
        {
            ptrDataType->isReadOnly = true;
        }
        DataTypeToken *token = gZtoonArena.Allocate<DataTypeToken>();
        *token = *(dataTypeToken->pointerDesc->dataTypeToken);
        token->readOnly = nullptr;
        ptrDataType->dataType = GetDataType(token);
        if (!ptrDataType->dataType)
        {
            ReportError(std::format("Unkown type '{}'", token->ToString()),
                        token->GetCodeErrString());
        }

        datatypesMap[ptrDataType->ToString()] = ptrDataType;

        ptrDataType->isReadOnly = dataTypeToken->readOnly;
        result = ptrDataType;
    }
    else if (dataTypeToken->readOnly)
    {
        DataType *type = gZtoonArena.Allocate<DataType>();
        *type = *(datatypesMap[dataTypeToken->GetDataType()->GetLexeme()]);

        type->isReadOnly = true;
        datatypesMap[type->ToString()] = type;
        result = type;
    }
    if (!result)
    {
        CodeErrString ces;
        ces.firstToken = dataTypeToken->GetFirstToken();
        ces.str = ces.firstToken->GetLexeme();
        ReportError("Unkown type", ces);
    }

    return result;
}

Scope::Scope(SemanticAnalyzer *semanticAnalyzer, Scope *parent)
{
    this->parent = parent;
    this->semanticAnalyzer = semanticAnalyzer;
    datatypesMap["i8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i8"]->type = DataType::Type::I8;

    datatypesMap["i16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i16"]->type = DataType::Type::I16;

    datatypesMap["i32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i32"]->type = DataType::Type::I32;

    datatypesMap["i64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["i64"]->type = DataType::Type::I64;

    datatypesMap["u8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u8"]->type = DataType::Type::U8;

    datatypesMap["u16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u16"]->type = DataType::Type::U16;

    datatypesMap["u32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u32"]->type = DataType::Type::U32;

    datatypesMap["u64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["u64"]->type = DataType::Type::U64;

    datatypesMap["f32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f32"]->type = DataType::Type::F32;
    datatypesMap["f64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["f64"]->type = DataType::Type::F64;

    datatypesMap["bool"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["bool"]->type = DataType::Type::BOOL;

    datatypesMap["notype"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["notype"]->type = DataType::Type::NOTYPE;

    datatypesMap["readonly i8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly i8"]->type = DataType::Type::I8;
    datatypesMap["readonly i8"]->isReadOnly = true;

    datatypesMap["readonly i16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly i16"]->type = DataType::Type::I16;
    datatypesMap["readonly i16"]->isReadOnly = true;

    datatypesMap["readonly i32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly i32"]->type = DataType::Type::I32;
    datatypesMap["readonly i32"]->isReadOnly = true;

    datatypesMap["readonly i64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly i64"]->type = DataType::Type::I64;
    datatypesMap["readonly i64"]->isReadOnly = true;

    datatypesMap["readonly u8"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly u8"]->type = DataType::Type::U8;
    datatypesMap["readonly u8"]->isReadOnly = true;

    datatypesMap["readonly u16"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly u16"]->type = DataType::Type::U16;
    datatypesMap["readonly u16"]->isReadOnly = true;

    datatypesMap["readonly u32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly u32"]->type = DataType::Type::U32;
    datatypesMap["readonly u32"]->isReadOnly = true;

    datatypesMap["readonly u64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly u64"]->type = DataType::Type::U64;
    datatypesMap["readonly u64"]->isReadOnly = true;

    datatypesMap["readonly f32"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly f32"]->type = DataType::Type::F32;
    datatypesMap["readonly f32"]->isReadOnly = true;

    datatypesMap["readonly f64"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly f64"]->type = DataType::Type::F64;
    datatypesMap["readonly f64"]->isReadOnly = true;

    datatypesMap["readonly bool"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["readonly bool"]->type = DataType::Type::BOOL;
    datatypesMap["readonly bool"]->isReadOnly = true;

    datatypesMap["notype"] = gZtoonArena.Allocate<DataType>();
    datatypesMap["notype"]->type = DataType::Type::NOTYPE;

    auto nullPtr = gZtoonArena.Allocate<PointerDataType>();
    nullPtr->type = DataType::Type::POINTER;
    nullPtr->dataType = datatypesMap["notype"];
    nullPtr->isReadOnly = true;
    nullPtr->isNullPtr = true;
    datatypesMap["nullptr"] = nullPtr;

    PointerDataType *strType = gZtoonArena.Allocate<PointerDataType>();
    strType->type = DataType::Type::POINTER;
    strType->dataType = datatypesMap["i8"];
    strType->isReadOnly = true;
    datatypesMap["readonly i8*"] = strType;
}

bool DataType::IsNumerical()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::F32:
    case Type::F64:
    case Type::ENUM:
        return true;
    default:
        return false;
    }
}
bool DataType::IsInteger()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::ENUM:
        return true;
    default:
        return false;
    }
}
bool DataType::IsFloat()
{
    switch (type)
    {
    case Type::F64:
    case Type::F32:
        return true;
    default:
        return false;
    }
}

bool DataType::IsSigned()
{
    switch (type)
    {
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
        return true;
    default:
        return false;
    }
}

TokenType DataType::ToTokenType()
{
    switch (type)
    {
    case DataType::Type::I8:
        return TokenType::I8;
    case DataType::Type::I16:
        return TokenType::I16;
    case DataType::Type::I32:
        return TokenType::I32;
    case DataType::Type::I64:
        return TokenType::I64;
    case DataType::Type::U8:
        return TokenType::U8;
    case DataType::Type::U16:
        return TokenType::U16;
    case DataType::Type::U32:
        return TokenType::U32;
    case DataType::Type::U64:
        return TokenType::U64;
    case DataType::Type::F32:
        return TokenType::F32;
    case DataType::Type::F64:
        return TokenType::F64;
    case DataType::Type::BOOL:
        return TokenType::BOOL;
    case DataType::Type::NOTYPE:
        return TokenType::NOTYPE;
    case DataType::Type::STRUCT:
    case DataType::Type::ENUM:
    case DataType::Type::UNION:
        return TokenType::IDENTIFIER;
    default:
        return TokenType::UNKNOWN;
    }
}

Symbol *Scope::GetSymbol(std::string name, CodeErrString codeErrString)
{

    Symbol *symbol = nullptr;

    Scope const *current = this;
    while (!symbol)
    {
        if (current->symbolsMap.contains(name))
        {
            symbol = current->symbolsMap.at(name);
        }

        for (auto pkgScope : current->importedPackages)
        {
            if (pkgScope->symbolsMap.contains(name))
            {
                symbol = pkgScope->symbolsMap.at(name);
                break;
            }
        }
        // go up one level;
        if (!current->parent || !lookUpParent)
        {
            // no more scopes.
            break;
        }
        current = current->parent;
    }

    if (symbol)
    {
        return symbol;
    }
    else
    {
        ReportError(std::format("'{}' is not defined in this scope.", name),
                    codeErrString);
    }
    return nullptr;
}
void Scope::AddSymbol(Symbol *symbol, CodeErrString codeErrString)
{
    if (symbolsMap.contains(symbol->GetName()))
    {
        Symbol *alreadyDefined = symbolsMap[symbol->GetName()];
        ReportError(std::format("'{}' already defined", symbol->GetName()),
                    codeErrString);
    }
    else
    {
        symbolsMap[symbol->GetName()] = symbol;
    }
}

void SemanticAnalyzer::ValidateAssignValueToVarArray(Expression *expr,
                                                     ArrayDataType *arrType)
{
    auto initListExpr = dynamic_cast<InitializerListExpression *>(expr);
    auto rValueArrType = dynamic_cast<ArrayDataType *>(exprToDataTypeMap[expr]);
    if (initListExpr)
    {
        if (!arrType->sizeExpr)
        {
            arrType->size = initListExpr->GetExpressions().size();
        }
        auto innerType = dynamic_cast<ArrayDataType *>(arrType->dataType);
        size_t index = 0;
        for (auto elementExpr : initListExpr->GetExpressions())
        {
            if (innerType)
            {
                // recurse
                ValidateAssignValueToVarArray(elementExpr, innerType);
            }
            else
            {
                auto leftExpr = gZtoonArena.Allocate<PrimaryExpression>();
                leftExpr->primary =
                    gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                leftExpr->isLvalue = true;
                exprToDataTypeMap[leftExpr] = arrType->dataType;
                Expression *e = leftExpr;

                DataType::Type type = DecideDataType(&e, &elementExpr);
                if (type == DataType::Type::UNKNOWN)
                {
                    ReportError(
                        std::format("Types '{}' and '{}' are not compatible",
                                    arrType->dataType->ToString(),
                                    exprToDataTypeMap[elementExpr]->ToString()),
                        elementExpr->GetCodeErrString());
                }

                initListExpr->expressions[index] = elementExpr;
            }
            index++;
        }
    }
    else if (rValueArrType)
    {
        if (rValueArrType->ToString() != arrType->ToString())
        {
            ReportError(std::format("Types '{}' and '{}' are not compatible",
                                    arrType->ToString(),
                                    rValueArrType->ToString()),
                        expr->GetCodeErrString());
        }
    }
}

void SemanticAnalyzer::ValidateAssignValueToVarStruct(
    Expression *expr, StructDataType *structType)
{

    auto initListExpr = dynamic_cast<InitializerListExpression *>(expr);
    auto rValueStructType =
        dynamic_cast<StructDataType *>(exprToDataTypeMap[expr]);
    if (initListExpr)
    {
        if (initListExpr->GetExpressions().empty())
        {
            initListExpr->GetExpressions().resize(structType->fields.size(), 0);
            return;
        }

        if (structType->fields.size() != initListExpr->expressions.size())
        {
            ReportError(
                std::format(
                    "List expression does not match struct datatype '{}'",
                    structType->name),
                expr->GetCodeErrString());
        }

        for (size_t index = 0; index < structType->fields.size(); index++)
        {
            auto listElement = initListExpr->expressions[index];
            auto structField = structType->fields[index];

            if (structField->GetType() == DataType::Type::STRUCT)
            {
                ValidateAssignValueToVarStruct(
                    listElement, dynamic_cast<StructDataType *>(structField));
            }
            else if (structField->GetType() == DataType::Type::ARRAY)
            {
                ValidateAssignValueToVarArray(
                    listElement, dynamic_cast<ArrayDataType *>(structField));
            }
            else
            {
                auto leftExpr = gZtoonArena.Allocate<PrimaryExpression>();
                leftExpr->primary =
                    gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                leftExpr->isLvalue = true;
                exprToDataTypeMap[leftExpr] = structField;
                Expression *e = leftExpr;

                DataType::Type type = DecideDataType(&e, &listElement);
                if (type == DataType::Type::UNKNOWN)
                {
                    ReportError(
                        std::format("Types '{}' and '{}' are not compatible",
                                    structField->ToString(),
                                    exprToDataTypeMap[listElement]->ToString()),
                        listElement->GetCodeErrString());
                }
                initListExpr->expressions[index] = listElement;
            }
        }
    }
    else if (rValueStructType)
    {
        if (rValueStructType->ToString() != structType->ToString())
        {
            ReportError(std::format("Types '{}' and '{}' are not compatible",
                                    structType->ToString(),
                                    rValueStructType->ToString()),
                        expr->GetCodeErrString());
        }
    }
}

SemanticAnalyzer::SemanticAnalyzer(std::vector<Package *> _packages,
                                   std::vector<Library *> _libraries)
    : packages(_packages), libraries(_libraries)
{
    currentStage = Stage::SEMANTIC_ANALYZER;

    std::function<void(Library * lib)> analyzeLib;
    analyzeLib = [&](Library *lib)
    {
        Analyze(lib->packages);
        for (auto l : lib->libs)
        {
            analyzeLib(l);
        }
        lib->analyed = true;
    };

    for (auto lib : libraries)
    {
        LibraryDataType *libType = gZtoonArena.Allocate<LibraryDataType>();
        libType->type = DataType::Type::LIBRARY;
        libType->name = lib->name;
        libType->lib = lib;
        libToDataTypeMap[lib] = libType;
        if (!lib->analyed)
            analyzeLib(lib);
    }

    Analyze(packages);
}
SemanticAnalyzer::~SemanticAnalyzer() {}
void SemanticAnalyzer::AnalyzePackage(Package *pkg)
{
    if (pkgToScopeMap.contains(pkg))
    {
        currentPackage = pkg;
        currentScope = pkgToScopeMap[pkg];
        return;
    }
    PackageDataType *pkgType = gZtoonArena.Allocate<PackageDataType>();

    pkgToDataTypeMap[pkg] = pkgType;

    pkgType->type = PackageDataType::Type::PACKAGE;
    pkgType->name = pkg->identifier->GetLexeme();
    pkgType->pkg = pkg;

    currentScope = gZtoonArena.Allocate<Scope>(this);
    pkgToScopeMap[pkg] = currentScope;

    CodeErrString ces;
    ces.firstToken = pkg->identifier;
    ces.str = ces.firstToken->GetLexeme();
    currentScope->AddSymbol(pkgType, ces);
    currentPackage = pkg;

    for (auto stmt : pkg->GetStatements())
    {
        if (dynamic_cast<ImportStatement *>(stmt))
        {
            auto importStmt = dynamic_cast<ImportStatement *>(stmt);
            AnalyzeImportStatement(importStmt);
        }
    }

    AnalyzePackageGlobalTypes(pkg);
    AnalyzePackageGlobalFuncsAndVars(pkg);
    AnalyzePackageGlobalTypeBodies(pkg);
}
void SemanticAnalyzer::Analyze(std::vector<Package *> &packages)
{
    for (auto pkg : packages)
    {
        AnalyzePackage(pkg);
        AnalyzePackageVarAndFuncBodies(pkg);
    }
}

void SemanticAnalyzer::AnalyzePackageGlobalTypes(Package *pkg)
{
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<StructStatement *>(stmt))
        {
            auto structStmt = dynamic_cast<StructStatement *>(stmt);
            AnalyzeStructStatement(structStmt, true, true, false);
        }
        else if (dynamic_cast<EnumStatement *>(stmt))
        {
            auto enumStmt = dynamic_cast<EnumStatement *>(stmt);
            AnalyzeEnumStatement(enumStmt, true, true, false);
        }
        else if (dynamic_cast<UnionStatement *>(stmt))
        {
            auto unionStmt = dynamic_cast<UnionStatement *>(stmt);
            AnalyzeUnionStatement(unionStmt, true, true, false);
        }
    }
}

void SemanticAnalyzer::AnalyzePackageGlobalFuncsAndVars(Package *pkg)
{
    currentPackage = pkg;
    currentScope = pkgToScopeMap[pkg];
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<VarDeclStatement *>(stmt))
        {
            VarDeclStatement *varDeclStatement =
                dynamic_cast<VarDeclStatement *>(stmt);
            AnalyzeVarDeclStatement(varDeclStatement, true, true, false);
        }
        else if (dynamic_cast<FnStatement *>(stmt))
        {
            FnStatement *fnStmt = dynamic_cast<FnStatement *>(stmt);
            AnalyzeFnStatement(fnStmt, true, true, false);
        }
    }
}

void SemanticAnalyzer::AnalyzePackageGlobalTypeBodies(Package *pkg)
{
    currentPackage = pkg;
    currentScope = pkgToScopeMap[pkg];
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<StructStatement *>(stmt))
        {
            auto structStmt = dynamic_cast<StructStatement *>(stmt);
            AnalyzeStructStatement(structStmt, true, false, true);
        }
        else if (dynamic_cast<EnumStatement *>(stmt))
        {
            auto enumStmt = dynamic_cast<EnumStatement *>(stmt);
            AnalyzeEnumStatement(enumStmt, true, false, true);
        }
        else if (dynamic_cast<UnionStatement *>(stmt))
        {
            auto unionStmt = dynamic_cast<UnionStatement *>(stmt);
            AnalyzeUnionStatement(unionStmt, true, false, true);
        }
    }
}
void SemanticAnalyzer::AnalyzePackageVarAndFuncBodies(Package *pkg)
{
    currentPackage = pkg;
    currentScope = pkgToScopeMap[pkg];
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<VarDeclStatement *>(stmt))
        {
            VarDeclStatement *varDeclStatement =
                dynamic_cast<VarDeclStatement *>(stmt);
            AnalyzeVarDeclStatement(varDeclStatement, true, false, true);
        }
        else if (dynamic_cast<FnStatement *>(stmt))
        {
            FnStatement *fnStmt = dynamic_cast<FnStatement *>(stmt);
            AnalyzeFnStatement(fnStmt, true, false, true);
        }
    }
}
void SemanticAnalyzer::PreAnalyzeStatement(Statement *statement, size_t index)
{
    if (dynamic_cast<ForLoopStatement *>(statement))
    {
        BlockStatement *blockStatement = gZtoonArena.Allocate<BlockStatement>();
        blockStatement->statements.push_back(statement);
        currentPackage->statements.at(index) = blockStatement;
    }
}

void SemanticAnalyzer::AnalyzeStatement(Statement *statement)
{
    if (dynamic_cast<ImportStatement *>(statement))
    {
        auto importStmt = dynamic_cast<ImportStatement *>(statement);
        AnalyzeImportStatement(importStmt);
    }
    else if (dynamic_cast<StructStatement *>(statement))
    {
        auto structStmt = dynamic_cast<StructStatement *>(statement);
        AnalyzeStructStatement(structStmt, false, true, true);
    }
    else if (dynamic_cast<UnionStatement *>(statement))
    {
        auto unionStmt = dynamic_cast<UnionStatement *>(statement);
        AnalyzeUnionStatement(unionStmt, false, true, true);
    }
    else if (dynamic_cast<EnumStatement *>(statement))
    {
        auto enumStmt = dynamic_cast<EnumStatement *>(statement);
        AnalyzeEnumStatement(enumStmt, false, true, true);
    }
    else if (dynamic_cast<VarDeclStatement *>(statement))
    {
        VarDeclStatement *varDeclStatement =
            dynamic_cast<VarDeclStatement *>(statement);
        AnalyzeVarDeclStatement(varDeclStatement, false, true, true);
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {
        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);
        AnalyzeVarAssignmentStatement(varAssignmentStatement);
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {
        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);
        AnalyzeVarCompundAssignmentStatement(varComAssignStatement);
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {
        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);
        AnalyzeBlockStatement(blockStatement);
    }
    else if (dynamic_cast<SwitchStatement *>(statement))
    {
        auto switchStmt = dynamic_cast<SwitchStatement *>(statement);
        AnalyzeSwitchStatement(switchStmt);
    }
    else if (dynamic_cast<IfStatement *>(statement))
    {
        IfStatement *ifStatement = dynamic_cast<IfStatement *>(statement);
        AnalyzeIfStatement(ifStatement);
    }
    else if (dynamic_cast<ElseIfStatement *>(statement))
    {
        ElseIfStatement *elifStatement =
            dynamic_cast<ElseIfStatement *>(statement);

        AnalyzeElseIfStatement(elifStatement);
    }
    else if (dynamic_cast<ElseStatement *>(statement))
    {
        ElseStatement *elseStatement = dynamic_cast<ElseStatement *>(statement);
        AnalyzeElseStatement(elseStatement);
    }
    else if (dynamic_cast<ExpressionStatement *>(statement))
    {
        ExpressionStatement *exprStatement =
            dynamic_cast<ExpressionStatement *>(statement);
        AnalyzeExpressionStatement(exprStatement);
    }
    else if (dynamic_cast<WhileLoopStatement *>(statement))
    {
        WhileLoopStatement *whileStatement =
            dynamic_cast<WhileLoopStatement *>(statement);
        AnalyzeWhileLoopStatement(whileStatement);
    }
    else if (dynamic_cast<ForLoopStatement *>(statement))
    {
        ForLoopStatement *forLoopStatement =
            dynamic_cast<ForLoopStatement *>(statement);
        AnalyzeForLoopStatement(forLoopStatement);
    }
    else if (dynamic_cast<BreakStatement *>(statement))
    {
        auto bStmt = dynamic_cast<BreakStatement *>(statement);
        AnalyzeBreakStatement(bStmt);
    }
    else if (dynamic_cast<ContinueStatement *>(statement))
    {
        // only in loops
        auto cStmt = dynamic_cast<ContinueStatement *>(statement);
        AnalyzeContinueStatement(cStmt);
    }
    else if (dynamic_cast<RetStatement *>(statement))
    {
        RetStatement *retStmt = dynamic_cast<RetStatement *>(statement);
        AnalyzeRetStatement(retStmt);
    }
}

void SemanticAnalyzer::AnalyzeBlockStatement(BlockStatement *blockStmt)
{
    Scope *scope = nullptr;
    if (!blockToScopeMap.contains(blockStmt))
    {
        scope = gZtoonArena.Allocate<Scope>(this, currentScope);
        blockToScopeMap[blockStmt] = scope;
    }
    else
    {
        scope = blockToScopeMap[blockStmt];
    }

    Scope *temp = currentScope;

    BlockStatement *blockTemp = currentBlockStatement;
    currentBlockStatement = blockStmt;

    currentScope = scope;

    for (size_t i = 0; i < blockStmt->statements.size(); i++)
    {
        AnalyzeStatement(blockStmt->statements[i]);
        blockStmt->index = i;
    }
    currentScope = temp;
    currentBlockStatement = blockTemp;
}

void SemanticAnalyzer::AnalyzeFnStatement(FnStatement *fnStmt, bool isGlobal,
                                          bool analyzeSymbol, bool analyzeBody,
                                          bool isMethod)
{
    if (analyzeSymbol)
    {
        Function *fp = gZtoonArena.Allocate<Function>();
        fp->name = fnStmt->identifier->GetLexeme();
        FnDataType *fpDataType = gZtoonArena.Allocate<FnDataType>();
        fpDataType->type = DataType::Type::FN;
        fpDataType->returnDataType =
            currentScope->GetDataType(fnStmt->returnDataTypeToken);
        fpDataType->isVarArgs = fnStmt->IsVarArgs();
        fpDataType->isMethod = fnStmt->method;
        // fp->fnStmt = fnStmt;
        fp->fnPointer = fpDataType->GetFnPtrType();

        currentScope->AddSymbol(fp, fnStmt->GetCodeErrString());
        Function *temp = currentFunction;
        currentFunction = fp;

        auto tempScope = currentScope;

        currentScope = gZtoonArena.Allocate<Scope>(this);
        currentScope->parent = tempScope;
        if (isMethod && fnStmt->IsPrototype())
        {
            ReportError("Methods must have a definition",
                        fnStmt->GetCodeErrString());
        }
        if (!fnStmt->IsPrototype())
        {
            blockToScopeMap[fnStmt->blockStatement] = currentScope;
        }

        for (Statement *p : fnStmt->parameters)
        {
            AnalyzeStatement(p);
            fpDataType->paramters.push_back(stmtToDataTypeMap[p]);
        }
        currentScope = tempScope;
        currentScope->datatypesMap[fp->fnPointer->ToString()] = fp->fnPointer;
        currentFunction = temp;
    }
    if (analyzeBody)
    {
        Function *fp = dynamic_cast<Function *>(currentScope->GetSymbol(
            fnStmt->GetIdentifier()->GetLexeme(), fnStmt->GetCodeErrString()));

        Function *temp = currentFunction;
        currentFunction = fp;
        AnalyzeStatement(fnStmt->blockStatement);
        currentFunction = temp;
        if (isGlobal)
        {
        }
        if (!fnStmt->IsPrototype())
        {
            std::function<bool(BlockStatement * bStmt)> checkRet =
                [&](BlockStatement *bStmt) -> bool
            {
                bool retFound = false;
                for (Statement *s : bStmt->statements)
                {
                    BlockStatement *blockStmt =
                        dynamic_cast<BlockStatement *>(s);
                    if (blockStmt)
                    {
                        checkRet(blockStmt);
                    }
                    // if stmt? or loops?
                    IfStatement *ifStmt = dynamic_cast<IfStatement *>(s);
                    RetStatement *retStmt = dynamic_cast<RetStatement *>(s);

                    if (ifStmt)
                    {

                        for (Statement *stmt :
                             ifStmt->GetBlockStatement()->statements)
                        {
                            RetStatement *retStmt =
                                dynamic_cast<RetStatement *>(stmt);
                            if (retStmt)
                            {
                                retFound = true;
                                break;
                            }
                        }

                        if (!retFound)
                        {
                            retFound = checkRet(ifStmt->GetBlockStatement());
                        }
                        if (retFound)
                        {
                            if (ifStmt->GetNextElseIforElseStatements().empty())
                            {
                                retFound = false;
                            }
                            bool foundInOtherPaths = false;
                            for (Statement *stmt :
                                 ifStmt->GetNextElseIforElseStatements())
                            {
                                auto elIfStmt =
                                    dynamic_cast<ElseIfStatement *>(stmt);
                                auto elseStmt =
                                    dynamic_cast<ElseStatement *>(stmt);
                                if (elIfStmt)
                                {
                                    foundInOtherPaths =
                                        checkRet(elIfStmt->GetBlockStatement());
                                }
                                else if (elseStmt)
                                {
                                    foundInOtherPaths =
                                        checkRet(elseStmt->GetBlockStatement());
                                }
                                if (!foundInOtherPaths)
                                {
                                    ReportError("Not all paths return.",
                                                fnStmt->GetCodeErrString());
                                }
                            }

                            retFound = foundInOtherPaths;
                            if (!retFound)
                            {
                                ReportError("Not all paths return.",
                                            fnStmt->GetCodeErrString());
                            }
                        }
                    }
                    else if (retStmt)
                    {
                        return true;
                    }
                }
                return retFound;
            };
            if (fnStmt->returnDataTypeToken->GetDataType()->type !=
                TokenType::NOTYPE)
            {
                if (fnStmt->GetBlockStatement()->statements.empty())
                {
                    ReportError(
                        std::format("Function '{}' does not return an "
                                    "expression.",
                                    fnStmt->GetIdentifier()->GetLexeme()),
                        fnStmt->GetCodeErrString());
                }
                // see if function ends with return statement.
                RetStatement *retStmt = dynamic_cast<RetStatement *>(
                    fnStmt->GetBlockStatement()->statements.back());
                if (!retStmt)
                {
                    bool retFoundInMainBlock = false;
                    for (size_t i;
                         i < fnStmt->GetBlockStatement()->statements.size();
                         i++)
                    {
                        RetStatement *ret = dynamic_cast<RetStatement *>(
                            fnStmt->GetBlockStatement()->statements[i]);
                        if (ret)
                        {
                            retFoundInMainBlock = true;
                        }
                    }
                    if (!retFoundInMainBlock)
                    {

                        if (!checkRet(fnStmt->GetBlockStatement()))
                        {
                            ReportError("Not all paths return.",
                                        fnStmt->GetCodeErrString());
                        }
                    }
                }
            }
        }
    }
}

void SemanticAnalyzer::AnalyzeVarDeclStatement(VarDeclStatement *varDeclStmt,
                                               bool isGlobal,
                                               bool analyzeSymbol,
                                               bool analyzeBody)
{
    if (analyzeSymbol)
    {
        stmtToDataTypeMap[varDeclStmt] =
            currentScope->GetDataType(varDeclStmt->GetDataType());

        Variable *var = gZtoonArena.Allocate<Variable>(
            varDeclStmt->GetIdentifier()->GetLexeme());
        var->token = varDeclStmt->GetIdentifier();
        var->dataType = stmtToDataTypeMap[varDeclStmt];
        var->varDeclStmt = varDeclStmt;
        currentScope->AddSymbol(var, varDeclStmt->GetCodeErrString());
    }
    if (analyzeBody)
    {
        if (varDeclStmt->GetExpression())
        {
            if (isGlobal)
            {
                auto pe = dynamic_cast<PrimaryExpression *>(
                    varDeclStmt->GetExpression());
                if (pe && pe->GetPrimary()->GetType() == TokenType::IDENTIFIER)
                {
                    std::function<bool(std::string, std::string)>
                        checkCycleVar = nullptr;
                    std::vector<VarDeclStatement *> cycleVarDecls;
                    checkCycleVar = [&](std::string toCheckName,
                                        std::string varName) -> bool
                    {
                        bool result = false;

                        Variable *rValueVar =
                            dynamic_cast<Variable *>(currentScope->GetSymbol(
                                varName, pe->GetCodeErrString()));
                        if (rValueVar)
                        {
                            cycleVarDecls.push_back(rValueVar->varDeclStmt);
                            if (rValueVar->GetName() == toCheckName)
                            {
                                result = true;
                            }
                            else
                            {
                                auto rPE = dynamic_cast<PrimaryExpression *>(
                                    rValueVar->varDeclStmt->GetExpression());
                                if (rPE && rPE->GetPrimary()->GetType() ==
                                               TokenType::IDENTIFIER)
                                {
                                    result = checkCycleVar(
                                        toCheckName,
                                        rPE->GetPrimary()->GetLexeme());
                                }
                            }
                        }
                        return result;
                    };

                    if (checkCycleVar(varDeclStmt->GetIdentifier()->GetLexeme(),
                                      pe->GetPrimary()->GetLexeme()))
                    {
                        std::string msg = std::format(
                            "Cycle variable assignment detected at '{}'\n",
                            varDeclStmt->GetCodeErrString().str);
                        for (auto v : cycleVarDecls)
                        {
                            msg +=
                                std::format("{}\n", v->GetCodeErrString().str);
                        }
                        ReportError(msg, varDeclStmt->GetCodeErrString());
                    }
                }
            }

            if (stmtToDataTypeMap[varDeclStmt]->GetType() ==
                DataType::Type::UNION)
            {
                ReportError("Cannot initialize union variable",
                            varDeclStmt->GetCodeErrString());
            }

            AnalyzeExpression(varDeclStmt->GetExpression());
            auto arrType =
                dynamic_cast<ArrayDataType *>(stmtToDataTypeMap[varDeclStmt]);
            auto structType =
                dynamic_cast<StructDataType *>(stmtToDataTypeMap[varDeclStmt]);
            if (arrType)
            {
                if (arrType && arrType->sizeExpr)
                {
                    auto innerPtrType = arrType;
                    while (innerPtrType)
                    {
                        if (innerPtrType->sizeExpr)
                        {
                            AnalyzeExpression(innerPtrType->sizeExpr);
                        }
                        innerPtrType = dynamic_cast<ArrayDataType *>(
                            innerPtrType->dataType);
                    }
                }
                else if (arrType &&
                         dynamic_cast<ArrayDataType *>(
                             exprToDataTypeMap[varDeclStmt->GetExpression()]))
                {
                    auto exprArrayType = dynamic_cast<ArrayDataType *>(
                        exprToDataTypeMap[varDeclStmt->GetExpression()]);

                    arrType->sizeExpr = varDeclStmt->GetExpression();
                }
                ValidateAssignValueToVarArray(varDeclStmt->GetExpression(),
                                              arrType);
            }
            else if (structType)
            {
                ValidateAssignValueToVarStruct(varDeclStmt->GetExpression(),
                                               structType);
            }
            else
            {
                PrimaryExpression *varExpr =
                    gZtoonArena.Allocate<PrimaryExpression>();
                varExpr->primary = varDeclStmt->GetIdentifier();
                varExpr->isLvalue = true;
                auto dt = stmtToDataTypeMap[varDeclStmt];
                exprToDataTypeMap[varExpr] = stmtToDataTypeMap[varDeclStmt];
                dt = exprToDataTypeMap[varExpr];
                Expression *variableRawExpression = varExpr;

                // check if types are compatible.
                DataType::Type dataType = DecideDataType(
                    &(variableRawExpression), &varDeclStmt->expression);
                if (dataType == DataType::Type::UNKNOWN)
                {
                    ReportError(
                        std::format(
                            "Cannot assign value of type '{}' to "
                            "variable "
                            "of type '{}'",
                            exprToDataTypeMap[varDeclStmt->GetExpression()]
                                ->ToString(),
                            stmtToDataTypeMap[varDeclStmt]->ToString()),
                        varDeclStmt->GetCodeErrString());
                }
            }
            if (isGlobal)
            {
                bool isReadonly =
                    exprToDataTypeMap[varDeclStmt->GetExpression()]
                        ->IsReadOnly();
                bool isListType =
                    exprToDataTypeMap[varDeclStmt->GetExpression()]
                        ->GetType() == DataType::Type::InitList;
                if (!isReadonly && !isListType)
                {
                    ReportError(
                        std::format(
                            "Only readonly or compile time expression are "
                            "allowd to be "
                            "assigned to global variables"),
                        varDeclStmt->GetExpression()->GetCodeErrString());
                }
            }
        }
    }
}
void SemanticAnalyzer::AnalyzeVarAssignmentStatement(
    VarAssignmentStatement *varAssignmentStmt)
{
    AnalyzeExpression(varAssignmentStmt->GetLValue());

    if (varAssignmentStmt->GetLValue()->IsLValue())
    {
        stmtToDataTypeMap[varAssignmentStmt] =
            exprToDataTypeMap[varAssignmentStmt->GetLValue()];
        if (stmtToDataTypeMap[varAssignmentStmt]->IsReadOnly())
        {

            ReportError("Cannot assign value to readonly variable",
                        varAssignmentStmt->GetCodeErrString());
        }
    }
    else
    {
        ReportError("Cannot assign value to r-value expression",
                    varAssignmentStmt->GetCodeErrString());
    }

    AnalyzeExpression(varAssignmentStmt->GetRValue());
    ArrayDataType *arrType = dynamic_cast<ArrayDataType *>(
        exprToDataTypeMap[varAssignmentStmt->GetLValue()]);
    StructDataType *structType = dynamic_cast<StructDataType *>(
        exprToDataTypeMap[varAssignmentStmt->GetLValue()]);
    if (arrType)
    {
        ValidateAssignValueToVarArray(varAssignmentStmt->GetRValue(), arrType);
    }
    else if (structType)
    {
        ValidateAssignValueToVarStruct(varAssignmentStmt->GetRValue(),
                                       structType);
    }
    else
    {
        DataType::Type dataType = DecideDataType(&(varAssignmentStmt->lValue),
                                                 &varAssignmentStmt->rValue);
        if (dataType == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Cannot assign value of type '{}' to variable "
                    "of type '{}'",
                    exprToDataTypeMap[varAssignmentStmt->rValue]->ToString(),
                    stmtToDataTypeMap[varAssignmentStmt]->ToString()),
                varAssignmentStmt->GetCodeErrString());
        }
    }
}
void SemanticAnalyzer::AnalyzeVarCompundAssignmentStatement(
    VarCompoundAssignmentStatement *varComStmt)
{
    AnalyzeExpression(varComStmt->GetLValue());
    if (varComStmt->GetLValue()->IsLValue())
    {

        stmtToDataTypeMap[varComStmt] =
            exprToDataTypeMap[varComStmt->GetLValue()];
        if (stmtToDataTypeMap[varComStmt]->IsReadOnly())
        {

            ReportError("Cannot assign value to readonly variable",
                        varComStmt->GetCodeErrString());
        }
    }
    else
    {
        ReportError("Cannot assign value to r-value expression",
                    varComStmt->GetCodeErrString());
    }

    AnalyzeExpression(varComStmt->GetRValue());

    DataType::Type dataType =
        DecideDataType(&(varComStmt->lValue), &varComStmt->rValue);
    if (dataType == DataType::Type::UNKNOWN)
    {
        ReportError(
            std::format("Cannot assign value of type '{}' to variable "
                        "of type '{}'",
                        exprToDataTypeMap[varComStmt->rValue]->ToString(),
                        stmtToDataTypeMap[varComStmt]->ToString()),
            varComStmt->GetCodeErrString());
    }
}
void SemanticAnalyzer::AnalyzeExpressionStatement(ExpressionStatement *exprStmt)
{
    AnalyzeExpression(exprStmt->GetExpression());
    stmtToDataTypeMap[exprStmt] = exprToDataTypeMap[exprStmt->GetExpression()];
}
void SemanticAnalyzer::AnalyzeIfStatement(IfStatement *ifStmt)
{
    AnalyzeExpression(ifStmt->GetExpression());

    if (exprToDataTypeMap[ifStmt->GetExpression()]->GetType() !=
        DataType::Type::BOOL)
    {
        ReportError(std::format("Expression after 'if' must be boolean."),
                    ifStmt->GetCodeErrString());
    }

    AnalyzeStatement(ifStmt->GetBlockStatement());

    for (Statement *s : ifStmt->GetNextElseIforElseStatements())
    {
        AnalyzeStatement(s);
    }
}
void SemanticAnalyzer::AnalyzeElseIfStatement(ElseIfStatement *elifStmt)
{
    AnalyzeExpression(elifStmt->GetExpression());

    if (exprToDataTypeMap[elifStmt->GetExpression()]->GetType() !=
        DataType::Type::BOOL)
    {
        ReportError(std::format("Expression after 'if' must be boolean."),
                    elifStmt->GetCodeErrString());
    }
    AnalyzeStatement(elifStmt->GetBlockStatement());
}
void SemanticAnalyzer::AnalyzeElseStatement(ElseStatement *elseStmt)
{
    AnalyzeStatement(elseStmt->GetBlockStatement());
}
void SemanticAnalyzer::AnalyzeSwitchStatement(SwitchStatement *switchStmt)
{
    if (switchStmt->GetCases().empty() && switchStmt->defualtCase)
    {
        ReportError("Cannot have only default case",
                    switchStmt->GetCodeErrString());
    }
    AnalyzeExpression(switchStmt->matchExpr);
    IfStatement *ifStmt = gZtoonArena.Allocate<IfStatement>();
    bool first = true;

    auto tokenEQEQ = gZtoonArena.Allocate<Token>(TokenType::EQUAL_EQUAL);
    tokenEQEQ->lexeme = "==";
    auto tokenOR = gZtoonArena.Allocate<Token>(TokenType::OR);
    tokenOR->lexeme = "||";
    for (auto c : switchStmt->GetCases())
    {
        std::vector<BinaryExpression *> expressions;

        for (auto expr : c->exprs)
        {
            auto bExpr = gZtoonArena.Allocate<BinaryExpression>();
            bExpr->op = tokenEQEQ;
            bExpr->left = switchStmt->matchExpr;
            bExpr->right = expr;

            expressions.push_back(bExpr);
        }

        BinaryExpression *prevBExpr = nullptr;
        if (expressions.size() > 1)
        {
            std::function<BinaryExpression *(BinaryExpression *)> buildExpr =
                nullptr;

            size_t index = 0;
            BinaryExpression *currentBExpr =
                gZtoonArena.Allocate<BinaryExpression>();
            currentBExpr->op = tokenOR;
            currentBExpr->left = expressions[index];
            index++;
            buildExpr = [&](BinaryExpression *bExpr) -> BinaryExpression *
            {
                BinaryExpression *result = nullptr;
                if (bExpr && !bExpr->right)
                {

                    if ((index + 1) < expressions.size())
                    {
                        bExpr->right = buildExpr(nullptr);
                    }
                    else
                    {
                        bExpr->right = expressions[index];
                        index++;
                    }
                    result = bExpr;
                }
                else
                {
                    result = gZtoonArena.Allocate<BinaryExpression>();
                    result->op = tokenOR;
                    result->left = expressions[index];
                    index++;
                    buildExpr(result);
                }

                return result;
            };

            prevBExpr = buildExpr(currentBExpr);
        }
        else
        {
            prevBExpr = expressions[0];
        }

        if (first)
        {
            ifStmt->expression = prevBExpr;
            ifStmt->blockStatement = c->blockStatement;
        }
        else
        {
            ElseIfStatement *elifStmt = gZtoonArena.Allocate<ElseIfStatement>();
            elifStmt->blockStatement = c->blockStatement;
            elifStmt->expression = prevBExpr;

            ifStmt->nextElseIforElseStatements.push_back(elifStmt);
        }

        first = false;
    }

    if (switchStmt->defualtCase)
    {
        ElseStatement *elseStmt = gZtoonArena.Allocate<ElseStatement>();
        elseStmt->blockStatement = switchStmt->defualtCase->blockStatement;
        ifStmt->nextElseIforElseStatements.push_back(elseStmt);
    }
    AnalyzeStatement(ifStmt);
    for (size_t index = 0; index < currentBlockStatement->statements.size();
         index++)
    {
        if (currentBlockStatement->statements[index] == switchStmt)
        {
            currentBlockStatement->statements[index] = ifStmt;
            break;
        }
    }
}
void SemanticAnalyzer::AnalyzeWhileLoopStatement(
    WhileLoopStatement *whileLoopStmt)
{
    AnalyzeExpression(whileLoopStmt->GetCondition());

    if (exprToDataTypeMap[whileLoopStmt->GetCondition()]->GetType() !=
        DataType::Type::BOOL)
    {
        ReportError(
            std::format("Expect expression '{}' to be boolean",
                        whileLoopStmt->GetCondition()->GetCodeErrString().str),
            whileLoopStmt->GetCondition()->GetCodeErrString());
    }
    inLoop++;
    AnalyzeStatement(whileLoopStmt->GetBlockStatement());
    inLoop--;
}
void SemanticAnalyzer::AnalyzeForLoopStatement(ForLoopStatement *forLoopStmt)
{
    if (forLoopStmt->GetInit() != nullptr)
        AnalyzeStatement(forLoopStmt->GetInit());
    if (forLoopStmt->GetCondition() != nullptr)
    {
        AnalyzeExpression(forLoopStmt->GetCondition());

        if (exprToDataTypeMap[forLoopStmt->GetCondition()]->GetType() !=
            DataType::Type::BOOL)
        {
            ReportError(
                std::format(
                    "Expect expression '{}' to be boolean",
                    forLoopStmt->GetCondition()->GetCodeErrString().str),
                forLoopStmt->GetCondition()->GetCodeErrString());
        }
    }
    if (forLoopStmt->GetUpdate())
    {
        AnalyzeStatement(forLoopStmt->GetUpdate());
    }
    inLoop++;

    AnalyzeStatement(forLoopStmt->GetBlockStatement());
    inLoop--;
}
void SemanticAnalyzer::AnalyzeBreakStatement(BreakStatement *breakStmt)
{ // only in loops and switch stmts
    if (inLoop == 0)
    {
        ReportError("'break' statement can only be used inside loop statements",
                    breakStmt->GetCodeErrString());
    }
}
void SemanticAnalyzer::AnalyzeContinueStatement(ContinueStatement *continueStmt)
{
    if (inLoop == 0)
    {
        ReportError(
            "'continue' statement can only be used inside loop statements",
            continueStmt->GetCodeErrString());
    }
}
void SemanticAnalyzer::AnalyzeStructStatement(StructStatement *structStmt,
                                              bool isGlobal, bool analyzeSymbol,
                                              bool analyzeBody)
{
    if (analyzeSymbol)
    {
        StructDataType *structType = gZtoonArena.Allocate<StructDataType>();
        structType->structStmt = structStmt;
        structType->type = DataType::Type::STRUCT;
        structType->complete = false;
        if (structStmt->identifier)
        {
            structType->name = structStmt->identifier->GetLexeme();
        }
        else
        {
            structType->name = std::format("__anonymous_struct__number__{}",
                                           (size_t)(structStmt));
        }
        currentScope->datatypesMap[structType->name] = structType;
        currentScope->AddSymbol(structType, structStmt->GetCodeErrString());
        stmtToDataTypeMap[structStmt] = structType;
        structType->fullName = std::format(
            "{}::{}",
            currentLibrary
                ? std::format("{}::{}", currentLibrary->name,
                              currentPackage->GetIdentifier()->GetLexeme())
                : currentPackage->GetIdentifier()->GetLexeme(),
            structType->name);
    }
    if (analyzeBody)
    {
        auto structType =
            dynamic_cast<StructDataType *>(stmtToDataTypeMap[structStmt]);
        Scope *scope = gZtoonArena.Allocate<Scope>(this, currentScope);
        Scope *temp = currentScope;

        currentScope = scope;
        structType->scope = currentScope;
        structType->defaultValuesList =
            gZtoonArena.Allocate<InitializerListExpression>();
        size_t index = 0;
        for (auto field : structStmt->fields)
        {
            AnalyzeStatement(field);
            auto fieldDataType = stmtToDataTypeMap[field];
            auto fieldStructType =
                dynamic_cast<StructDataType *>(fieldDataType);
            auto fieldUnionType = dynamic_cast<UnionDataType *>(fieldDataType);

            if (fieldStructType && !field->GetExpression())
            {
                if (!field->GetExpression())
                {
                    structType->defaultValuesList->expressions.push_back(
                        fieldStructType->defaultValuesList);
                }
                else
                {
                    structType->defaultValuesList->expressions.push_back(
                        field->GetExpression());
                }
            }
            else
            {
                structType->defaultValuesList->expressions.push_back(
                    field->GetExpression());
            }
            if (isGlobal)
            {
                std::function<bool(std::string, StructStatement *,
                                   UnionStatement *)>
                    checkCycleType = nullptr;
                std::vector<VarDeclStatement *> nestedField;
                checkCycleType = [&](std::string structName,
                                     StructStatement *structStmt,
                                     UnionStatement *unionStmt) -> bool
                {
                    bool result = false;
                    if (structStmt)
                    {
                        for (auto f : structStmt->GetFields())
                        {
                            auto fStructType = dynamic_cast<StructDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            if (fStructType)
                            {
                                nestedField.push_back(f);
                                if (fStructType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result = checkCycleType(
                                        structName, fStructType->structStmt,
                                        nullptr);
                                }
                            }
                            else if (fUnionType)
                            {
                                nestedField.push_back(f);
                                if (fUnionType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result =
                                        checkCycleType(structName, nullptr,
                                                       fUnionType->unionStmt);
                                }
                            }
                        }
                    }
                    else if (unionStmt)
                    {
                        for (auto fStmt : unionStmt->GetFields())
                        {
                            auto f = dynamic_cast<VarDeclStatement *>(fStmt);
                            if (!f)
                                continue;
                            auto fStructType = dynamic_cast<StructDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            if (fStructType)
                            {
                                nestedField.push_back(f);
                                if (fStructType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result = checkCycleType(
                                        structName, fStructType->structStmt,
                                        nullptr);
                                }
                            }
                            else if (fUnionType)
                            {
                                nestedField.push_back(f);
                                if (fUnionType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result =
                                        checkCycleType(structName, nullptr,
                                                       fUnionType->unionStmt);
                                }
                            }
                        }
                    }

                    return result;
                };

                auto fieldStructStmt =
                    fieldStructType ? fieldStructType->structStmt : nullptr;
                auto fieldUnionStmt =
                    fieldUnionType ? fieldUnionType->unionStmt : nullptr;

                if (checkCycleType(structType->GetName(), fieldStructStmt,
                                   fieldUnionStmt))
                {
                    std::string msg = std::format(
                        "Cycle field in struct '{}'\n", structType->GetName());
                    msg += std::format("{}\n", field->GetCodeErrString().str);
                    std::string prevType =
                        dynamic_cast<VarDeclStatement *>(field)
                            ->GetDataType()
                            ->ToString();
                    for (auto f : nestedField)
                    {
                        msg += std::format("In '{}'\n", prevType);
                        msg += std::format("{}\n", f->GetCodeErrString().str);
                        prevType = f->GetDataType()->ToString();
                    }
                    ReportError(std::format("'{}'", msg),
                                field->GetCodeErrString());
                }
            }

            structType->fields.push_back(fieldDataType);
            index++;
        }

        InitListType *listType = gZtoonArena.Allocate<InitListType>();
        listType->type = DataType::Type::InitList;

        listType->dataTypes = structType->fields;

        exprToDataTypeMap[structType->defaultValuesList] = listType;

        for (auto method : structStmt->methods)
        {
            AnalyzeFnStatement(method, false, true, false, true);
        }

        for (auto method : structStmt->methods)
        {
            FnStatement *fnStmt = method;
            AnalyzeFnStatement(method, false, false, true, true);
        }

        structType->complete = true;
        currentScope = temp;
    }
}
void SemanticAnalyzer::AnalyzeUnionStatement(UnionStatement *unionStmt,
                                             bool isGlobal, bool analyzeSymbol,
                                             bool analyzeBody)
{

    if (analyzeSymbol)
    {
        UnionDataType *unionType = gZtoonArena.Allocate<UnionDataType>();
        unionType->unionStmt = unionStmt;
        unionType->type = DataType::Type::UNION;
        unionType->name = unionStmt->identifier->GetLexeme();

        currentScope->datatypesMap[unionType->name] = unionType;
        currentScope->AddSymbol(unionType, unionStmt->GetCodeErrString());
        stmtToDataTypeMap[unionStmt] = unionType;
    }
    if (analyzeBody)
    {
        auto unionType =
            dynamic_cast<UnionDataType *>(stmtToDataTypeMap[unionStmt]);

        Scope *scope = gZtoonArena.Allocate<Scope>(this, currentScope);
        Scope *temp = currentScope;

        currentScope = scope;
        unionType->scope = currentScope;
        std::vector<VarDeclStatement *> toAddFields;
        for (auto field : unionStmt->fields)
        {
            AnalyzeStatement(field);
            StructStatement *fieldStructStmt =
                dynamic_cast<StructStatement *>(field);

            if (fieldStructStmt)
            {
                for (auto f : fieldStructStmt->fields)
                {
                    toAddFields.push_back(f);
                }
            }

            auto fieldDataType = stmtToDataTypeMap[field];
            auto fieldUnionType = dynamic_cast<UnionDataType *>(fieldDataType);

            auto fieldStructType =
                dynamic_cast<StructDataType *>(fieldDataType);
            if (fieldStructType)
            {
                for (auto symbol : fieldStructType->scope->symbolsMap)
                {
                    currentScope->symbolsMap[symbol.first] = symbol.second;
                }
            }

            if (isGlobal)
            {

                std::function<bool(std::string, StructStatement *,
                                   UnionStatement *)>
                    checkCycleType = nullptr;
                std::vector<VarDeclStatement *> nestedField;
                checkCycleType = [&](std::string structName,
                                     StructStatement *structStmt,
                                     UnionStatement *unionStmt) -> bool
                {
                    bool result = false;
                    if (structStmt)
                    {
                        for (auto f : structStmt->GetFields())
                        {
                            auto fStructType = dynamic_cast<StructDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            if (fStructType)
                            {
                                nestedField.push_back(f);
                                if (fStructType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result = checkCycleType(
                                        structName, fStructType->structStmt,
                                        nullptr);
                                }
                            }
                            else if (fUnionType)
                            {

                                if (fUnionType->GetName() == structName)
                                {
                                    nestedField.push_back(f);
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    nestedField.push_back(f);
                                    result =
                                        checkCycleType(structName, nullptr,
                                                       fUnionType->unionStmt);
                                }
                            }
                            if (!result)
                                nestedField.clear();
                        }
                    }
                    else if (unionStmt)
                    {
                        for (auto fStmt : unionStmt->GetFields())
                        {
                            auto f = dynamic_cast<VarDeclStatement *>(fStmt);
                            if (!f)
                                continue;
                            auto fStructType = dynamic_cast<StructDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                currentScope->GetDataType(f->GetDataType()));
                            if (fStructType)
                            {
                                nestedField.push_back(f);
                                if (fStructType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result = checkCycleType(
                                        structName, fStructType->structStmt,
                                        nullptr);
                                }
                            }
                            else if (fUnionType)
                            {
                                nestedField.push_back(f);
                                if (fUnionType->GetName() == structName)
                                {
                                    result = true;
                                    break;
                                }
                                else
                                {
                                    result =
                                        checkCycleType(structName, nullptr,
                                                       fUnionType->unionStmt);
                                }
                            }
                            if (!result)
                                nestedField.clear();
                        }
                    }

                    return result;
                };

                auto fieldUnionStmt =
                    fieldUnionType ? fieldUnionType->unionStmt : nullptr;

                auto fieldStructStatement =
                    fieldStructType ? fieldStructType->structStmt : nullptr;
                if (checkCycleType(unionType->GetName(), fieldStructStatement,
                                   fieldUnionStmt))
                {
                    std::string msg = std::format("Cycle field in union '{}'\n",
                                                  unionType->GetName());
                    msg += std::format("{}\n", field->GetCodeErrString().str);
                    std::string prevType =
                        dynamic_cast<VarDeclStatement *>(field)
                            ->GetDataType()
                            ->ToString();
                    for (auto f : nestedField)
                    {
                        msg += std::format("In '{}'\n", prevType);
                        msg += std::format("{}\n", f->GetCodeErrString().str);
                        prevType = f->GetDataType()->ToString();
                    }
                    ReportError(std::format("'{}'", msg),
                                field->GetCodeErrString());
                }
            }

            unionType->fields.push_back(fieldDataType);
        }
        for (auto f : toAddFields)
        {
            unionStmt->fields.push_back(f);
        }
    }
}
void SemanticAnalyzer::AnalyzeEnumStatement(EnumStatement *enumStmt,
                                            bool isGlobal, bool analyzeSymbol,
                                            bool analyzeBody)
{

    if (analyzeSymbol)
    {
        EnumDataType *enumType = gZtoonArena.Allocate<EnumDataType>();
        enumType->type = DataType::Type::ENUM;
        enumType->name = enumStmt->identifier->GetLexeme();
        enumType->enumStmt = enumStmt;
        enumType->datatype = currentScope->GetDataType(enumStmt->datatype);

        if (!enumType->datatype->IsInteger())
        {
            ReportError("Enum type must be interger",
                        enumStmt->GetCodeErrString());
        }

        stmtToDataTypeMap[enumStmt] = enumType;

        currentScope->datatypesMap[enumType->name] = enumType;
        currentScope->AddSymbol(enumType, enumStmt->GetCodeErrString());
    }
    if (analyzeBody)
    {
        auto enumType =
            dynamic_cast<EnumDataType *>(stmtToDataTypeMap[enumStmt]);

        for (auto f : enumStmt->fields)
        {
            if (f->expr)
            {
                AnalyzeExpression(f->expr);
                auto fDataType = exprToDataTypeMap[f->expr];
                if (!fDataType->IsInteger())
                {
                    ReportError("Only integer types are allowed",
                                f->expr->GetCodeErrString());
                }
            }
        }
    }
}
void SemanticAnalyzer::AnalyzeRetStatement(RetStatement *retStmt)
{
    Function *fn = (Function *)currentFunction;
    // retStmt->fnStmt = fn->GetFnStatement();
    stmtToDataTypeMap[retStmt] =
        fn->GetFnDataTypeFromFnPTR()->GetReturnDataType();
    if (retStmt->expression)
    {
        AnalyzeExpression(retStmt->GetExpression());

        PrimaryExpression *primaryExpr =
            gZtoonArena.Allocate<PrimaryExpression>();
        primaryExpr->primary = gZtoonArena.Allocate<Token>(
            fn->GetFnDataTypeFromFnPTR()->GetReturnDataType()->ToTokenType());
        Expression *expr = primaryExpr;
        exprToDataTypeMap[expr] =
            fn->GetFnDataTypeFromFnPTR()->GetReturnDataType();
        retStmt->expression->isLvalue = false;
        DataType::Type type = DecideDataType(&expr, &retStmt->expression);
        fn->retStmt = retStmt;
        // retStmt->fnStmt = fn->GetFnStatement();
        if (type == DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format("Return expression '{}' is not compatible with "
                            "function return type '{}'",
                            retStmt->GetExpression()->GetCodeErrString().str,
                            currentFunction->GetFnDataTypeFromFnPTR()
                                ->returnDataType->ToString()),
                retStmt->GetCodeErrString());
        }
    }
    else
    {
        DataType::Type type = DataType::Type::NOTYPE;
        if (type != currentFunction->GetFnDataTypeFromFnPTR()
                        ->GetReturnDataType()
                        ->GetType())
        {
            ReportError(
                std::format("Return expression '{}' is not compatible with "
                            "function return type '{}'",
                            retStmt->GetExpression()
                                ? retStmt->GetCodeErrString().str
                                : TokenDataTypeToString(TokenType::NOTYPE),
                            currentFunction->GetFnDataTypeFromFnPTR()
                                ->returnDataType->ToString()),
                retStmt->GetCodeErrString());
        }
        else if (retStmt->expression)
        {

            ReportError(std::format("Function '{}' does not return a value.",
                                    currentFunction->GetFnDataTypeFromFnPTR()
                                        ->returnDataType->ToString()),
                        retStmt->GetCodeErrString());
        }
    }
}
void SemanticAnalyzer::AnalyzeImportStatement(ImportStatement *importStmt)
{
    auto pe =
        dynamic_cast<PrimaryExpression *>(importStmt->GetPackageExpression());
    auto maExpr = dynamic_cast<MemberAccessExpression *>(
        importStmt->GetPackageExpression());
    if (maExpr)
    {
        auto peLeft =
            dynamic_cast<PrimaryExpression *>(maExpr->GetLeftExpression());
        if (!peLeft)
        {
            ReportError(std::format("Expected library identiifer"),
                        maExpr->GetLeftExpression()->GetCodeErrString());
        }
        auto peRight =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        Library *lib = nullptr;
        for (auto l : libraries)
        {
            if (l->name == peLeft->GetPrimary()->GetLexeme())
            {
                lib = l;
                break;
            }
        }

        if (!lib)
        {
            ReportError(std::format("Library '{}' is not found",
                                    peLeft->GetPrimary()->GetLexeme()),
                        peLeft->GetCodeErrString());
        }
        Package *pkg = nullptr;
        for (auto p : lib->packages)
        {
            if (p->GetIdentifier()->GetLexeme() ==
                peRight->GetPrimary()->GetLexeme())
            {
                pkg = p;
                break;
            }
        }

        if (!pkg)
        {
            ReportError(std::format("Package '{}' is not found in library '{}'",
                                    peLeft->GetPrimary()->GetLexeme(),
                                    lib->name),
                        peRight->GetCodeErrString());
        }

        if (!lib->analyed)
        {
            auto tempPackages = packages;
            auto tempPkg = currentPackage;
            auto temp = currentLibrary;
            currentLibrary = lib;
            packages = tempPackages;
            Analyze(lib->packages);
            currentLibrary = temp;
            lib->analyed = true;
            currentPackage = tempPkg;
            currentScope = pkgToScopeMap[tempPkg];
            packages = tempPackages;
        }
        currentScope->importedPackages.push_back(pkgToScopeMap[pkg]);
    }
    else if (pe && pe->GetPrimary()->GetType() == TokenType::IDENTIFIER)
    {
        std::string importedName = pe->GetPrimary()->GetLexeme();
        // find pkg
        Package *importedPkg = nullptr;
        Library *importedLibrary = nullptr;

        for (auto p : packages)
        {
            if (p->GetIdentifier()->GetLexeme() == importedName)
            {
                importedPkg = p;
                break;
            }
        }

        if (!importedPkg)
        {
            for (auto lib : libraries)
            {
                if (lib->name == importedName)
                {
                    importedLibrary = lib;
                    break;
                }
            }
        }

        if (!importedPkg && !importedLibrary)

        {
            ReportError(std::format("Package or library '{}' is not found",
                                    importedName),
                        importStmt->GetPackageExpression()->GetCodeErrString());
        }
        if (importedPkg)
        {
            // check if package is analyzed
            if (!pkgToScopeMap.contains(importedPkg))
            {
                AnalyzePackage(importedPkg);
                // currentPackage = currentPackage;
                currentScope = pkgToScopeMap[currentPackage];
            }
            auto importedPkgScope = pkgToScopeMap[importedPkg];
            pkgToScopeMap[currentPackage]->importedPackages.push_back(
                importedPkgScope);
        }
        else if (importedLibrary)
        {
            if (!importedLibrary->analyed)
            {
                auto tempPackages = packages;
                auto tempPkg = currentPackage;
                auto temp = currentLibrary;
                currentLibrary = importedLibrary;
                packages = tempPackages;
                Analyze(importedLibrary->packages);
                currentLibrary = temp;
                importedLibrary->analyed = true;
                currentPackage = tempPkg;
                currentScope = pkgToScopeMap[tempPkg];
                packages = tempPackages;
            }
            for (auto impPkg : importedLibrary->packages)
            {
                auto importedPkgScope = pkgToScopeMap[impPkg];
                pkgToScopeMap[currentPackage]->importedPackages.push_back(
                    importedPkgScope);
            }
        }
    }
}

void SemanticAnalyzer::AnalyzeFnExpression(FnExpression *fnExpr)
{
    Function *fp = gZtoonArena.Allocate<Function>();
    fp->name = std::format("__anonymous_fn_{}", (size_t)fnExpr);
    fnExpr->name = fp->name;
    FnDataType *fpDataType = gZtoonArena.Allocate<FnDataType>();
    fpDataType->type = DataType::Type::FN;
    fpDataType->returnDataType =
        currentScope->GetDataType(fnExpr->returnDataTypeToken);
    fpDataType->isVarArgs = fnExpr->IsVarArgs();
    exprToDataTypeMap[fnExpr] = fpDataType;
    fp->fnPointer = fpDataType->GetFnPtrType();

    currentScope->AddSymbol(fp, fnExpr->GetCodeErrString());

    auto tempScope = currentScope;

    currentScope = gZtoonArena.Allocate<Scope>(this);
    currentScope->parent = pkgToScopeMap[currentPackage];

    blockToScopeMap[fnExpr->GetBlockStatement()] = currentScope;

    Function *temp = currentFunction;
    currentFunction = fp;
    for (Statement *p : fnExpr->parameters)
    {
        AnalyzeStatement(p);
        fpDataType->paramters.push_back(stmtToDataTypeMap[p]);
    }
    currentScope = tempScope;
    AnalyzeStatement(fnExpr->blockStatement);
    currentFunction = temp;
    // check for ret statement;
    if (fnExpr->IsPrototype())
    {
        ReportError(std::format("Anonymous function must have definition"),
                    fnExpr->GetCodeErrString());
    }
    std::function<bool(BlockStatement * bStmt)> checkRet =
        [&](BlockStatement *bStmt) -> bool
    {
        bool retFound = false;
        for (Statement *s : bStmt->statements)
        {
            BlockStatement *blockStmt = dynamic_cast<BlockStatement *>(s);
            if (blockStmt)
            {
                checkRet(blockStmt);
            }
            // if stmt? or loops?
            IfStatement *ifStmt = dynamic_cast<IfStatement *>(s);
            RetStatement *retStmt = dynamic_cast<RetStatement *>(s);

            if (ifStmt)
            {

                for (Statement *stmt : ifStmt->GetBlockStatement()->statements)
                {
                    RetStatement *retStmt = dynamic_cast<RetStatement *>(stmt);
                    if (retStmt)
                    {
                        retFound = true;
                        break;
                    }
                }

                if (!retFound)
                {
                    retFound = checkRet(ifStmt->GetBlockStatement());
                }
                if (retFound)
                {
                    if (ifStmt->GetNextElseIforElseStatements().empty())
                    {
                        retFound = false;
                    }
                    bool foundInOtherPaths = false;
                    for (Statement *stmt :
                         ifStmt->GetNextElseIforElseStatements())
                    {
                        auto elIfStmt = dynamic_cast<ElseIfStatement *>(stmt);
                        auto elseStmt = dynamic_cast<ElseStatement *>(stmt);
                        if (elIfStmt)
                        {
                            foundInOtherPaths =
                                checkRet(elIfStmt->GetBlockStatement());
                        }
                        else if (elseStmt)
                        {
                            foundInOtherPaths =
                                checkRet(elseStmt->GetBlockStatement());
                        }
                        if (!foundInOtherPaths)
                        {
                            ReportError("Not all paths return.",
                                        fnExpr->GetCodeErrString());
                        }
                    }

                    retFound = foundInOtherPaths;
                    if (!retFound)
                    {
                        ReportError("Not all paths return.",
                                    fnExpr->GetCodeErrString());
                    }
                }
            }
            else if (retStmt)
            {
                return true;
            }
        }
        return retFound;
    };
    if (fnExpr->returnDataTypeToken->GetDataType()->type != TokenType::NOTYPE)
    {
        if (fnExpr->GetBlockStatement()->statements.empty())
        {
            ReportError(
                std::format("Function '{}' does not return an expression.",
                            fp->GetName()),
                fnExpr->GetCodeErrString());
        }
        // see if function ends with return statement.
        RetStatement *retStmt = dynamic_cast<RetStatement *>(
            fnExpr->GetBlockStatement()->statements.back());
        if (!retStmt)
        {
            bool retFoundInMainBlock = false;
            for (size_t i; i < fnExpr->GetBlockStatement()->statements.size();
                 i++)
            {
                RetStatement *ret = dynamic_cast<RetStatement *>(
                    fnExpr->GetBlockStatement()->statements[i]);
                if (ret)
                {
                    retFoundInMainBlock = true;
                }
            }
            if (!retFoundInMainBlock)
            {

                if (!checkRet(fnExpr->GetBlockStatement()))
                {
                    ReportError("Not all paths return.",
                                fnExpr->GetCodeErrString());
                }
            }
        }
    }
    currentScope->datatypesMap[fp->fnPointer->ToString()] = fp->fnPointer;
}
void SemanticAnalyzer::AnalyzeTernaryExpression(TernaryExpression *ternaryExpr)
{
    AnalyzeExpression(ternaryExpr->condition);
    AnalyzeExpression(ternaryExpr->trueExpr);
    AnalyzeExpression(ternaryExpr->falseExpr);

    if (exprToDataTypeMap[ternaryExpr->condition]->GetType() !=
        DataType::Type::BOOL)
    {
        ReportError("Expected expression to be boolean type",
                    ternaryExpr->GetCodeErrString());
    }

    DataType::Type type =
        DecideDataType(&ternaryExpr->trueExpr, &ternaryExpr->falseExpr);

    if (type == DataType::Type::UNKNOWN)
    {
        ReportError(
            std::format("Expression '{}' and '{}' must be of the same type.",
                        ternaryExpr->trueExpr->GetCodeErrString().str,
                        ternaryExpr->falseExpr->GetCodeErrString().str),
            ternaryExpr->GetCodeErrString());
    }

    exprToDataTypeMap[ternaryExpr] = exprToDataTypeMap[ternaryExpr->trueExpr];
}
void SemanticAnalyzer::AnalyzeBinaryExpression(BinaryExpression *binaryExpr)
{
    AnalyzeExpression(binaryExpr->GetLeftExpression());
    AnalyzeExpression(binaryExpr->GetRightExpression());

    DataType *left = exprToDataTypeMap[binaryExpr->left];
    DataType *right = exprToDataTypeMap[binaryExpr->right];

    DataType::Type dataType =
        DecideDataType(&binaryExpr->left, &binaryExpr->right);

    exprToDataTypeMap[binaryExpr] = left;
    if (dataType == DataType::Type::UNKNOWN)
    {
        ReportError(
            std::format("Cannot perform '{}' on datatypes '{}' and '{}'",
                        binaryExpr->op->GetLexeme(), left->ToString(),
                        right->ToString()),
            binaryExpr->GetCodeErrString());
        return;
    }

    switch (binaryExpr->op->GetType())
    {
    case TokenType::PLUS:
    case TokenType::DASH:
    case TokenType::ASTERISK:
    case TokenType::SLASH:
    case TokenType::PERCENTAGE:
    {
        if (!left->IsNumerical())
        {
            ReportError(std::format("Left expression of '{}' must be "
                                    "numerical type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!right->IsNumerical())
        {
            ReportError(std::format("Right expression of '{}' must be "
                                    "numerical type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::BITWISE_AND:
    case TokenType::BITWISE_OR:
    case TokenType::BITWISE_XOR:
    case TokenType::SHIFT_LEFT:
    case TokenType::SHIFT_RIGHT:
    {
        if (!left->IsInteger())
        {
            ReportError(std::format("Left expression of '{}' must be "
                                    "integer type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!right->IsInteger())
        {
            ReportError(std::format("Right expression of '{}' must be "
                                    "integer type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::EQUAL_EQUAL:
    case TokenType::EXCLAMATION_EQUAL:
    case TokenType::LESS:
    case TokenType::LESS_EQUAL:
    case TokenType::GREATER:
    case TokenType::GREATER_EQUAL:
    {

        if (!left->IsNumerical() && left->type != DataType::Type::BOOL)
        {
            ReportError(std::format("Left expression of '{}' must be "
                                    "boolean or numerical type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!right->IsNumerical() && right->type != DataType::Type::BOOL)
        {
            ReportError(std::format("Right expression of '{}' must be "
                                    "boolean or numerical type",
                                    binaryExpr->op->GetLexeme()),
                        binaryExpr->GetCodeErrString());
        }
        exprToDataTypeMap[binaryExpr] = currentScope->datatypesMap["bool"];
        break;
    }
    case TokenType::OR:
    case TokenType::AND:
    {
        if (left->type != DataType::Type::BOOL)
        {
            ReportError(
                std::format("Left expression of '{}' must be boolean type",
                            binaryExpr->op->GetLexeme()),
                binaryExpr->GetCodeErrString());
        }
        else if (right->type != DataType::Type::BOOL)
        {
            ReportError(
                std::format("Right expression of '{}' must be boolean type",
                            binaryExpr->op->GetLexeme()),
                binaryExpr->GetCodeErrString());
        }
        exprToDataTypeMap[binaryExpr] = currentScope->datatypesMap["bool"];
        break;
    }

    default:
    {
        break;
    }
    }
}
void SemanticAnalyzer::AnalyzeCastExpression(CastExpression *castExpr)
{
    AnalyzeExpression(castExpr->GetExpression());
    castExpr->isLvalue = castExpr->GetExpression()->IsLValue();
    exprToDataTypeMap[castExpr] =
        currentScope->GetDataType(castExpr->castToTypeToken);

    DataType *valueType = exprToDataTypeMap[castExpr->expression];
    DataType *castType = exprToDataTypeMap[castExpr];
    if (castType->GetType() == DataType::Type::BOOL)
    {
        if (exprToDataTypeMap[castExpr]->GetType() != DataType::Type::BOOL)
        {
            ReportError(
                std::format("Cannot Cast from '{}' datatype to '{}' datatype",
                            valueType->ToString(), castType->ToString()),
                castExpr->GetCodeErrString());
        }
    }

    else if (castType->GetType() == DataType::Type::ARRAY)
    {
        ReportError(std::format("Cannot cast to array type"),
                    castExpr->GetCodeErrString());
    }
    else if ((valueType->GetType() == DataType::Type::ARRAY) &&
             castType->GetType() == DataType::Type::POINTER)
    {
        // check if the pointer is a pointer to the inner type of the
        // array.
        auto arrType = dynamic_cast<ArrayDataType *>(valueType);
        auto ptrType = dynamic_cast<PointerDataType *>(castType);

        if (arrType->dataType->ToString() != ptrType->dataType->ToString())
        {
            ReportError(std::format("Cannot cast from '{}' to '{}'",
                                    arrType->ToString(), ptrType->ToString()),
                        castExpr->GetCodeErrString());
        }
    }
    //  ptr stuff
    //  ptr to ptr ok
    //  int to ptr ok
    //  ptr to int ok
    //  else not ok.
    else if (castType->GetType() == DataType::Type::POINTER &&
             (valueType->GetType() != DataType::Type::POINTER &&
              !valueType->IsInteger()))
    {
        ReportError(
            std::format("Cannot Cast from '{}' datatype to '{}' datatype",
                        valueType->ToString(), castType->ToString()),
            castExpr->GetCodeErrString());
    }

    else if ((valueType->GetType() == DataType::Type::POINTER) &&
             !castType->IsInteger() &&
             castType->GetType() != DataType::Type::POINTER)
    {
        ReportError(std::format("Pointer datatype can only be casted to an "
                                "integer or pointer datatype"),
                    castExpr->GetCodeErrString());
    }
}
void SemanticAnalyzer::AnalyzeUnaryExpression(UnaryExpression *unaryExpr)
{
    DataType *rightDataType = nullptr;
    if (!unaryExpr->GetSizeOfDataTypeToken())
    {
        AnalyzeExpression(unaryExpr->GetRightExpression());
        rightDataType = exprToDataTypeMap[unaryExpr->right];
        exprToDataTypeMap[unaryExpr] = rightDataType;
    }
    else
    {
        exprToDataTypeMap[unaryExpr] = currentScope->datatypesMap["u64"];
    }

    switch (unaryExpr->GetOperator()->GetType())
    {
    case TokenType::DASH:
    {
        // numerical
        if (!rightDataType->IsNumerical() && !rightDataType->IsSigned())
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::TILDE:
    {
        if (!rightDataType->IsInteger())
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::DASH_DASH:
    case TokenType::PLUS_PLUS:
    {
        // numerical
        if (!unaryExpr->GetRightExpression()->IsLValue())
        {
            ReportError(
                std::format("Cannot perform unary operator '{}' on r-value "
                            "expression",
                            unaryExpr->GetOperator()->GetLexeme()),
                unaryExpr->GetRightExpression()->GetCodeErrString());
        }

        if (!rightDataType->IsNumerical())
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }
        if (unaryExpr->IsPostfix())
        {
            // Insert new variable assign statement with binary
            VarAssignmentStatement *varAssignStatement =
                gZtoonArena.Allocate<VarAssignmentStatement>();
            Token *typeToken =
                gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
            varAssignStatement->lValue = unaryExpr->GetRightExpression();

            BinaryExpression *binaryExpr =
                gZtoonArena.Allocate<BinaryExpression>();
            binaryExpr->left = unaryExpr->GetRightExpression();
            PrimaryExpression *oneExpr =
                gZtoonArena.Allocate<PrimaryExpression>();
            exprToDataTypeMap[oneExpr] = rightDataType;

            Token *oneToken = nullptr;
            if (rightDataType->IsInteger())
            {
                oneToken = gZtoonArena.Allocate<TokenLiteral<int32_t>>(
                    TokenType::INTEGER_LITERAL, 1);
            }
            else if (rightDataType->IsFloat())
            {
                oneToken = gZtoonArena.Allocate<TokenLiteral<float>>(
                    TokenType::FLOAT_LITERAL, 1.0);
            }
            oneExpr->primary = oneToken;
            binaryExpr->right = oneExpr;
            exprToDataTypeMap[binaryExpr] = rightDataType;
            binaryExpr->op =
                unaryExpr->GetOperator()->type == TokenType::PLUS_PLUS
                    ? gZtoonArena.Allocate<Token>(TokenType::PLUS)
                    : gZtoonArena.Allocate<Token>(TokenType::DASH);
            varAssignStatement->rValue = binaryExpr;
            // operation directly after this statement.
            // need to know if inside block statement or no.
            // if inside, need to get block statement.

            if (currentBlockStatement)
            {
                // inside
                size_t s = currentBlockStatement->statements.size();
                currentBlockStatement->statements.insert(
                    currentBlockStatement->statements.begin() +
                        currentBlockStatement->index + 1,
                    varAssignStatement);
            }
            // disable this expression.
            unaryExpr->op = gZtoonArena.Allocate<Token>(TokenType::PLUS);
        }

        break;
    }
    case TokenType::PLUS:
    {
        // numerical
        if (!rightDataType->IsNumerical())
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::EXCLAMATION:
    {
        // numerical
        if (rightDataType->type != DataType::Type::BOOL)
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }
        break;
    }
    case TokenType::SIZEOF:
    {
        if (rightDataType && rightDataType->type == DataType::Type::UNKNOWN &&
            !unaryExpr->sizeOfDataTypeToken)
        {
            ReportError(std::format("Cannot perform unary operator '{}' on "
                                    "datatype '{}'.",
                                    unaryExpr->GetOperator()->GetLexeme(),
                                    rightDataType->ToString()),
                        unaryExpr->GetCodeErrString());
        }

        break;
    }

    case TokenType::ASTERISK:
    {
        // deref works on ptrs.
        if (rightDataType->type != DataType::Type::POINTER)
        {
            ReportError(std::format("Cannot derefrence non-pointer types"),
                        unaryExpr->GetCodeErrString());
        }
        PointerDataType *ptrDataType =
            dynamic_cast<PointerDataType *>(rightDataType);
        unaryExpr->isLvalue = true;
        exprToDataTypeMap[unaryExpr] = ptrDataType->PointedToDatatype();
        break;
    }
    case TokenType::BITWISE_AND:
    {
        // ref only works on lvalue
        if (!unaryExpr->GetRightExpression()->IsLValue())
        {
            ReportError("Cannot reference r-value expressions",
                        unaryExpr->GetCodeErrString());
        }

        PointerDataType *ptrDataType = gZtoonArena.Allocate<PointerDataType>();
        ptrDataType->type = DataType::Type::POINTER;
        ptrDataType->dataType = rightDataType;
        currentScope->datatypesMap[ptrDataType->ToString()] = ptrDataType;
        ptrDataType->isReadOnly = true;
        exprToDataTypeMap[unaryExpr] = ptrDataType;
        break;
    }
    default:
    {
        ReportError(std::format("Unkown unary operator '{}'",
                                unaryExpr->GetOperator()->GetLexeme()),
                    unaryExpr->GetCodeErrString());
        break;
    }
    }
}
void SemanticAnalyzer::AnalyzeGroupingExpression(
    GroupingExpression *groupingEpxr)
{
    AnalyzeExpression(groupingEpxr->GetExpression());
    groupingEpxr->isLvalue = groupingEpxr->GetExpression()->IsLValue();
    exprToDataTypeMap[groupingEpxr] =
        exprToDataTypeMap[groupingEpxr->GetExpression()];
}
void SemanticAnalyzer::AnalyzeSubScriptExpression(SubscriptExpression *subExpr)
{
    AnalyzeExpression(subExpr->GetExpression());

    DataType *exprDataType = exprToDataTypeMap[subExpr->GetExpression()];
    auto ptrType = dynamic_cast<PointerDataType *>(exprDataType);
    auto arrType = dynamic_cast<ArrayDataType *>(exprDataType);
    if (!ptrType && !arrType)
    {
        ReportError("Subscript operator only works on pointer and array type",
                    subExpr->GetExpression()->GetCodeErrString());
    }
    AnalyzeExpression(subExpr->GetIndexExpression());
    DataType *indexDataType = exprToDataTypeMap[subExpr->GetIndexExpression()];
    if (!indexDataType->IsInteger())
    {
        ReportError("Index expression must be integer type",
                    subExpr->GetIndexExpression()->GetCodeErrString());
    }
    if (ptrType)
    {
        exprToDataTypeMap[subExpr] = ptrType->PointedToDatatype();
    }
    else if (arrType)
    {
        exprToDataTypeMap[subExpr] = arrType->dataType;
    }
}
void SemanticAnalyzer::AnalyzeFnCallExpression(FnCallExpression *fnCallExpr)
{
    AnalyzeExpression(fnCallExpr->GetGetExpression());
    // id or epxr
    PointerDataType *fnPtrType = dynamic_cast<PointerDataType *>(
        exprToDataTypeMap[fnCallExpr->GetGetExpression()]);
    FnDataType *fnDataType =
        dynamic_cast<FnDataType *>(fnPtrType ? fnPtrType->dataType : nullptr);

    if (!fnDataType)
    {
        ReportError("Expression is not of function pointer type",
                    fnCallExpr->GetCodeErrString());
    }
    exprToDataTypeMap[fnCallExpr] = fnDataType->GetReturnDataType();

    for (Expression *arg : fnCallExpr->args)
    {
        AnalyzeExpression(arg);
    }

    if (fnDataType->IsMethod() &&
        methodToCallerMap.contains(fnCallExpr->GetGetExpression()))
    {
        if (fnDataType->GetParameters()[0]->GetType() ==
            DataType::Type::POINTER)
        {
            UnaryExpression *unaryExpr =
                gZtoonArena.Allocate<UnaryExpression>();
            auto op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_AND);
            op->lexeme = "&";
            unaryExpr->op = op;
            unaryExpr->right =
                methodToCallerMap[fnCallExpr->GetGetExpression()];
            PointerDataType *ptrDataType =
                gZtoonArena.Allocate<PointerDataType>();
            ptrDataType->type = DataType::Type::POINTER;
            ptrDataType->dataType = exprToDataTypeMap[unaryExpr->right];
            exprToDataTypeMap[unaryExpr] = ptrDataType;
            fnCallExpr->GetArgs().insert(fnCallExpr->GetArgs().begin(),
                                         unaryExpr);
        }
        else
        {
            fnCallExpr->GetArgs().insert(
                fnCallExpr->GetArgs().begin(),
                methodToCallerMap[fnCallExpr->GetGetExpression()]);
        }
    }

    if (fnCallExpr->GetArgs().size() != fnDataType->GetParameters().size())
    {
        if (!fnDataType->IsVarArgs())
        {
            ReportError("function call expression arguments are not "
                        "compatible with function paramters",
                        fnCallExpr->GetCodeErrString());
        }
    }
    size_t index = 0;

    for (DataType *argDataType : fnDataType->GetParameters())
    {
        PrimaryExpression *id = gZtoonArena.Allocate<PrimaryExpression>();
        id->primary = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
        auto params = fnDataType->GetParameters();
        size_t size = params.size();
        exprToDataTypeMap[id] = params[index];
        Expression *exprId = id;
        exprId->isLvalue = true;
        if (DecideDataType(&exprId, &fnCallExpr->args[index]) ==
            DataType::Type::UNKNOWN)
        {
            ReportError(
                std::format(
                    "Argument '{}' of type '{}' is not compatible "
                    "with paramter of type '{}'",
                    fnCallExpr->args[index]->GetCodeErrString().str,
                    exprToDataTypeMap[fnCallExpr->args[index]]->ToString(),
                    params[index]->ToString()),
                fnCallExpr->args[index]->GetCodeErrString());
        }
        index++;
    }
}
void SemanticAnalyzer::AnalyzeInitializerListExpression(
    InitializerListExpression *initListExpr)
{
    bool allSameType = true;
    DataType *prevType = nullptr;
    InitListType *listType = gZtoonArena.Allocate<InitListType>();
    listType->type = DataType::Type::InitList;

    for (auto expr : initListExpr->expressions)
    {
        if (!expr)
        {
            continue;
        }
        AnalyzeExpression(expr);
        auto type = exprToDataTypeMap[expr];
        listType->dataTypes.push_back(type);
        if (prevType && allSameType)
        {
            if (prevType->type == type->type)
            {
                allSameType = true;
            }
            else
            {
                allSameType = false;
            }
        }
        prevType = type;
    }
    listType->allSameType = allSameType;
    exprToDataTypeMap[initListExpr] = listType;
}
void SemanticAnalyzer::AnalyzeMemberAccessExpression(
    MemberAccessExpression *maExpr)
{
    maExpr->isLvalue = true;

    Package *pkgFound = nullptr;
    Library *libFound = nullptr;
    auto primaryExpression =
        dynamic_cast<PrimaryExpression *>(maExpr->GetLeftExpression());

    if (primaryExpression &&
        primaryExpression->GetPrimary()->GetType() == TokenType::IDENTIFIER)
    {
        for (auto pkg : packages)
        {
            if (pkg->GetIdentifier()->GetLexeme() ==
                primaryExpression->GetPrimary()->GetLexeme())
            {
                pkgFound = pkg;
                break;
            }
        }
        if (pkgFound)
        {
            // See if pkg is analyzed
            if (!pkgToScopeMap.contains(pkgFound))
            {
                AnalyzePackage(pkgFound);
            }
            CodeErrString ces;
            ces.firstToken = pkgFound->GetIdentifier();
            ces.str = ces.firstToken->GetLexeme();
            auto pkgType = dynamic_cast<PackageDataType *>(
                pkgToScopeMap[pkgFound]->GetSymbol(
                    pkgFound->GetIdentifier()->GetLexeme(), ces));
            exprToDataTypeMap[maExpr->GetLeftExpression()] = pkgType;
        }
        if (!pkgFound)
        {
            for (auto lib : libraries)
            {
                if (lib->name == primaryExpression->GetPrimary()->GetLexeme())
                {
                    libFound = lib;
                    break;
                }
            }
            if (libFound)
            {
                maExpr->accessType =
                    MemberAccessExpression::AccessType::LIBRARY;
                auto libType = libToDataTypeMap[libFound];
                exprToDataTypeMap[maExpr->GetLeftExpression()] = libType;
            }
        }
    }
    if (!pkgFound && !libFound)
    {
        AnalyzeExpression(maExpr->GetLeftExpression());
    }

    DataType *left = exprToDataTypeMap[maExpr->GetLeftExpression()];

    if (maExpr->token->GetType() == TokenType::DOUBLE_COLON)
    {
        if (left->GetType() != DataType::Type::ENUM &&
            left->GetType() != DataType::Type::PACKAGE &&
            left->GetType() != DataType::Type::LIBRARY)
        {
            ReportError("'::' operator is only used on 'enum' and 'package'",
                        maExpr->GetCodeErrString());
        }
    }
    else if (maExpr->token->GetType() == TokenType::PERIOD)
    {
        if (left->GetType() == DataType::Type::ENUM ||
            left->GetType() == DataType::Type::PACKAGE)
        {
            ReportError("'.' operator is only used on 'struct' and 'union'",
                        maExpr->GetCodeErrString());
        }
    }
    if (left->GetType() == DataType::Type::PACKAGE)
    {
        maExpr->accessType = MemberAccessExpression::AccessType::PACKAGE;
        auto pkgType = dynamic_cast<PackageDataType *>(left);

        auto temp = currentScope;
        currentScope = pkgToScopeMap[pkgType->pkg];

        PrimaryExpression *packageMember =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        AnalyzeExpression(packageMember);
        currentScope = temp;
        exprToDataTypeMap[maExpr] = exprToDataTypeMap[packageMember];
    }
    else if (left->GetType() == DataType::Type::LIBRARY)
    {
        auto libType = dynamic_cast<LibraryDataType *>(left);

        PrimaryExpression *package =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());
        Package *pkgFound = nullptr;
        for (auto pkg : libType->lib->packages)
        {
            if (pkg->GetIdentifier()->GetLexeme() ==
                package->GetPrimary()->GetLexeme())
            {
                pkgFound = pkg;
                break;
            }
        }
        if (!pkgFound)
        {
            ReportError(std::format("Package '{}' is not found in library '{}'",
                                    package->GetPrimary()->GetLexeme(),
                                    libType->lib->name),
                        package->GetCodeErrString());
        }
        auto pkgType = pkgToDataTypeMap[pkgFound];
        exprToDataTypeMap[maExpr] = pkgType;
    }
    else if (left->GetType() == DataType::Type::ENUM)
    {
        maExpr->accessType = MemberAccessExpression::AccessType::ENUM;
        auto enumType = dynamic_cast<EnumDataType *>(left);
        auto enumStmt = enumType->enumStmt;

        PrimaryExpression *field =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        auto itr =
            std::find_if(enumStmt->fields.begin(), enumStmt->fields.end(),
                         [field](EnumStatement::Field *other) {
                             return field->primary->GetLexeme() ==
                                    other->identifier->GetLexeme();
                         });
        bool found = itr != enumStmt->fields.end();
        if (!found)
        {
            ReportError(std::format("'{}' field not found in enum '{}'",
                                    field->primary->GetLexeme(),
                                    enumType->GetName()),
                        maExpr->GetCodeErrString());
        }

        exprToDataTypeMap[maExpr] = left;
    }

    else if (left->type != DataType::Type::POINTER &&
             left->type != DataType::Type::STRUCT &&
             left->type != DataType::Type::UNION)
    {
        ReportError(std::format("Left expression of '.' must be type of "
                                "struct, union, or a pointer to one of them"),
                    maExpr->GetCodeErrString());
    }
    if (left->type == DataType::Type::POINTER)
    {
        auto leftPtrType = dynamic_cast<PointerDataType *>(left);
        if (leftPtrType->dataType->GetType() != DataType::Type::STRUCT &&
            leftPtrType->dataType->GetType() != DataType::Type::UNION)
        {
            ReportError(
                std::format("Left expression of '.' must be type of "
                            "struct, union, or a pointer to one of them"),
                maExpr->GetCodeErrString());
        }
        else
        {
            // change left expr type to deref type
            UnaryExpression *derefPtr = gZtoonArena.Allocate<UnaryExpression>();
            derefPtr->op = gZtoonArena.Allocate<Token>(TokenType::ASTERISK);
            derefPtr->postfix = true;
            derefPtr->right = maExpr->leftExpr;
            AnalyzeExpression(derefPtr);
            maExpr->leftExpr = derefPtr;
            left = exprToDataTypeMap[derefPtr];
        }
    }

    if (left->type == DataType::Type::STRUCT)
    {
        maExpr->accessType = MemberAccessExpression::AccessType::STRUCT;
        StructStatement *structStmt = nullptr;
        StructDataType *structType = nullptr;

        structType = dynamic_cast<StructDataType *>(left);

        if (left->type == DataType::Type::POINTER)
        {
            auto leftPtrType = dynamic_cast<PointerDataType *>(left);
            if (leftPtrType->dataType->GetType() == DataType::Type::STRUCT)
            {
                structType = dynamic_cast<StructDataType *>(
                    leftPtrType->PointedToDatatype());
            }
        }

        structStmt = structType->structStmt;

        PrimaryExpression *field =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        if (!field || field->primary->GetType() != TokenType::IDENTIFIER)
        {
            ReportError(
                std::format("Right expression of '.' must be an identifier"),
                maExpr->GetCodeErrString());
        }
        // Check if struct has this field
        auto fieldItr =
            std::find_if(structStmt->fields.begin(), structStmt->fields.end(),
                         [field](VarDeclStatement *f) {
                             return f->GetIdentifier()->GetLexeme() ==
                                    field->primary->GetLexeme();
                         });
        auto methodItr =
            std::find_if(structStmt->methods.begin(), structStmt->methods.end(),
                         [field](FnStatement *f) {
                             return f->GetIdentifier()->GetLexeme() ==
                                    field->primary->GetLexeme();
                         });
        if (fieldItr == structStmt->fields.end() &&
            methodItr == structStmt->methods.end())
        {
            ReportError(std::format("Struct type '{}' does not have "
                                    "field with name '{}'",
                                    structType->name,
                                    field->GetPrimary()->GetLexeme()),
                        field->GetCodeErrString());
        }

        Scope *temp = currentScope;
        currentScope = structType->scope;
        currentScope->lookUpParent = false;

        AnalyzeExpression(maExpr->GetRightExpression());
        DataType *rightDataType = exprToDataTypeMap[field];
        if (methodItr != structStmt->methods.end())
        {
            methodToCallerMap[maExpr] = maExpr->GetLeftExpression();
        }
        exprToDataTypeMap[maExpr] = rightDataType;
        currentScope = temp;
        currentScope->lookUpParent = true;
    }
    else if (left->type == DataType::Type::UNION)
    {
        maExpr->accessType = MemberAccessExpression::AccessType::UNION;
        UnionStatement *unionStmt = nullptr;
        UnionDataType *unionType = nullptr;

        unionType = dynamic_cast<UnionDataType *>(left);

        if (left->type == DataType::Type::POINTER)
        {
            auto leftPtrType = dynamic_cast<PointerDataType *>(left);
            if (leftPtrType->dataType->GetType() == DataType::Type::UNION)
            {
                unionType = dynamic_cast<UnionDataType *>(
                    leftPtrType->PointedToDatatype());
            }
        }

        unionStmt = unionType->unionStmt;

        PrimaryExpression *field =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        if (!field || field->primary->GetType() != TokenType::IDENTIFIER)
        {
            ReportError(
                std::format("Right expression of '.' must be an identifier"),
                maExpr->GetCodeErrString());
        }
        // Check if union has this field
        bool found = false;
        StructStatement *anonymousStruct = nullptr;
        for (auto f : unionStmt->fields)
        {
            auto varField = dynamic_cast<VarDeclStatement *>(f);
            auto structField = dynamic_cast<StructStatement *>(f);
            if (varField)
            {
                if (varField->identifier->GetLexeme() ==
                    field->primary->GetLexeme())
                {
                    found = true;
                }
            }
            else if (structField)
            {
                for (VarDeclStatement *structF : structField->fields)
                {
                    if (structF->identifier->GetLexeme() ==
                        field->GetPrimary()->GetLexeme())
                    {
                        found = true;
                        anonymousStruct = structField;
                        break;
                    }
                }
            }
            if (found)
            {
                break;
            }
        }
        if (!found)
        {
            ReportError(std::format("union type '{}' does not have "
                                    "field with name '{}'",
                                    unionType->name,
                                    field->GetPrimary()->GetLexeme()),
                        field->GetCodeErrString());
        }

        if (anonymousStruct)
        {
            maExpr->accessType = MemberAccessExpression::AccessType::STRUCT;
            MemberAccessExpression *expr =
                gZtoonArena.Allocate<MemberAccessExpression>();
            expr->accessType = MemberAccessExpression::AccessType::UNION;
            expr->leftExpr = maExpr->GetLeftExpression();
            expr->rightExpr = maExpr->GetRightExpression();
            exprToDataTypeMap[expr] = stmtToDataTypeMap[anonymousStruct];
            maExpr->leftExpr = expr;
        }

        Scope *temp = currentScope;
        currentScope = unionType->scope;
        currentScope->lookUpParent = false;

        AnalyzeExpression(maExpr->GetRightExpression());

        DataType *rightDataType = exprToDataTypeMap[field];
        exprToDataTypeMap[maExpr] = rightDataType;
        currentScope = temp;
        currentScope->lookUpParent = true;
    }
}
void SemanticAnalyzer::AnalyzePrimaryExpression(PrimaryExpression *primaryExpr)
{
    switch (primaryExpr->GetPrimary()->GetType())
    {
    case TokenType::INTEGER_LITERAL:
    {
        std::string type = "readonly i32";
        if (dynamic_cast<const TokenLiteral<uint32_t> *>(
                primaryExpr->GetPrimary()))
        {
            type = "readonly u32";
        }
        else if (dynamic_cast<const TokenLiteral<uint64_t> *>(
                     primaryExpr->GetPrimary()))
        {
            type = "readonly u64";
        }
        exprToDataTypeMap[primaryExpr] = currentScope->datatypesMap[type];
        break;
    }
    case TokenType::FLOAT_LITERAL:
    {
        exprToDataTypeMap[primaryExpr] =
            currentScope->datatypesMap["readonly f32"];
        break;
    }
    case TokenType::STRING_LITERAL:
    {
        auto strLiteral = dynamic_cast<TokenLiteral<std::string> const *>(
            primaryExpr->primary);
        exprToDataTypeMap[primaryExpr] =
            currentScope->datatypesMap["readonly i8*"];
        break;
    }
    case TokenType::CHARACTER_LITERAL:
    {
        exprToDataTypeMap[primaryExpr] =
            currentScope->datatypesMap["readonly i8"];
        break;
    }
    case TokenType::TRUE:
    case TokenType::FALSE:
    {
        exprToDataTypeMap[primaryExpr] =
            currentScope->datatypesMap["readonly bool"];
        break;
    }
    case TokenType::IDENTIFIER:
    {
        primaryExpr->isLvalue = true;

        exprToDataTypeMap[primaryExpr] =
            currentScope
                ->GetSymbol(primaryExpr->primary->GetLexeme(),
                            primaryExpr->GetCodeErrString())
                ->GetDataType();

        break;
    }
    case TokenType::NULL_PTR:
    {
        exprToDataTypeMap[primaryExpr] = currentScope->datatypesMap["nullptr"];
    }
    break;
    default:
    {
        break;
    }
    }
}

void removeReadonlyPrefix(std::string &str)
{
    const std::string prefix = "readonly ";
    const size_t prefixLen = prefix.length();

    if (str.compare(0, prefixLen, prefix) == 0)
    {
        str.erase(0, prefixLen);
    }
}
DataType::Type SemanticAnalyzer::DecideDataType(Expression **left,
                                                Expression **right)
{
    DataType *leftDataType = exprToDataTypeMap[*left];
    DataType *rightDataType = exprToDataTypeMap[*right];
    bool isLeftLValue = (*left)->IsLValue();
    bool isRightLValue = (*right)->IsLValue();

    if (leftDataType->type == DataType::Type::POINTER &&
        rightDataType->type == DataType::Type::POINTER)
    {
        if (dynamic_cast<PointerDataType *>(rightDataType)->isNullPtr)
        {
            exprToDataTypeMap[*right] = leftDataType;
            rightDataType = leftDataType;
        }
    }

    std::string leftDataTypeStr = leftDataType->ToString();
    std::string rightDataTypeStr = rightDataType->ToString();
    removeReadonlyPrefix(leftDataTypeStr);
    removeReadonlyPrefix(rightDataTypeStr);

    if (leftDataTypeStr != rightDataTypeStr)
    {
        if ((*left)->IsLValue() && !(*right)->IsLValue() &&
                (rightDataType->IsInteger()) ||
            rightDataType->IsFloat())
        {
            TokenType leftVarDataType = leftDataType->ToTokenType();
            TokenType rightLiteralDataType = rightDataType->ToTokenType();
            if (IsInteger(leftVarDataType) && IsInteger(rightLiteralDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *right;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(leftDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    leftDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                exprToDataTypeMap[castExpr] = leftDataType;
                exprToDataTypeMap[castExpr]->isReadOnly =
                    rightDataType->IsReadOnly();
                *right = castExpr;
                return leftDataType->type;
            }
            else if (IsFloat(leftVarDataType) && IsFloat(rightLiteralDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *right;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(leftDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    leftDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                exprToDataTypeMap[castExpr] = leftDataType;
                *right = castExpr;
                return leftDataType->type;
            }
            else
            {
                // error
                return DataType::Type::UNKNOWN;
            }
        }
        else if ((*right)->IsLValue() && !(*left)->IsLValue() &&
                 (leftDataType->IsInteger() || leftDataType->IsFloat()))
        {

            TokenType leftLiteralDataType = leftDataType->ToTokenType();
            TokenType rightVarDataType = rightDataType->ToTokenType();
            if (IsInteger(leftLiteralDataType) && IsInteger(rightVarDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *left;
                exprToDataTypeMap[castExpr] = rightDataType;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    rightDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                *left = castExpr;
                return rightDataType->type;
            }
            else if (IsFloat(leftLiteralDataType) && IsFloat(rightVarDataType))
            {
                // cast
                CastExpression *castExpr =
                    gZtoonArena.Allocate<CastExpression>();
                castExpr->expression = *left;
                exprToDataTypeMap[castExpr] = rightDataType;
                castExpr->castToTypeToken =
                    gZtoonArena.Allocate<DataTypeToken>();
                castExpr->castToTypeToken->dataType =
                    gZtoonArena.Allocate<Token>(rightDataType->ToTokenType());
                castExpr->castToTypeToken->readOnly =
                    rightDataType->IsReadOnly()
                        ? gZtoonArena.Allocate<Token>(TokenType::READONLY)
                        : nullptr;
                *left = castExpr;
                return rightDataType->type;
            }
            else
            {
                // error
                return DataType::Type::UNKNOWN;
            }
        }
        else
        {
            return DataType::Type::UNKNOWN;
        }
    }

    return leftDataType->type;
}

void SemanticAnalyzer::AnalyzeExpression(Expression *expression)
{
    if (dynamic_cast<FnExpression *>(expression))
    {
        FnExpression *fnExpr = dynamic_cast<FnExpression *>(expression);
        AnalyzeFnExpression(fnExpr);
    }
    else if (dynamic_cast<FnCallExpression *>(expression))
    {
        FnCallExpression *fnCallExpr =
            dynamic_cast<FnCallExpression *>(expression);
        AnalyzeFnCallExpression(fnCallExpr);
    }
    else if (dynamic_cast<MemberAccessExpression *>(expression))
    {
        auto maExpr = dynamic_cast<MemberAccessExpression *>(expression);
        AnalyzeMemberAccessExpression(maExpr);
    }
    else if (dynamic_cast<InitializerListExpression *>(expression))
    {
        auto initListExpr =
            dynamic_cast<InitializerListExpression *>(expression);
        AnalyzeInitializerListExpression(initListExpr);
    }
    else if (dynamic_cast<TernaryExpression *>(expression))
    {
        TernaryExpression *ternaryExpr =
            dynamic_cast<TernaryExpression *>(expression);
        AnalyzeTernaryExpression(ternaryExpr);
    }
    else if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        AnalyzeBinaryExpression(binaryExpression);
    }
    else if (dynamic_cast<UnaryExpression *>(expression))
    {
        UnaryExpression *unaryExpression =
            dynamic_cast<UnaryExpression *>(expression);
        AnalyzeUnaryExpression(unaryExpression);
    }
    else if (dynamic_cast<GroupingExpression *>(expression))
    {
        GroupingExpression *groupingExpression =
            dynamic_cast<GroupingExpression *>(expression);
        AnalyzeGroupingExpression(groupingExpression);
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        AnalyzeCastExpression(castExpression);
    }
    else if (dynamic_cast<SubscriptExpression *>(expression))
    {
        SubscriptExpression *subExpr =
            dynamic_cast<SubscriptExpression *>(expression);
        AnalyzeSubScriptExpression(subExpr);
    }
    else if (dynamic_cast<PrimaryExpression *>(expression))
    {
        PrimaryExpression *primaryExpression =
            dynamic_cast<PrimaryExpression *>(expression);
        AnalyzePrimaryExpression(primaryExpression);
    }
    else
    {
        ReportError("This expression is not supported.",
                    expression->GetCodeErrString());
    };
}
