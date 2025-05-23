#include "compiler/compiler.h"
#include "error_report.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semantic_analyzer.h"
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <functional>
#include <stack>
#include <unordered_map>
#include <utility>
#include <vector>
void removeReadonlyPrefix(std::string &str)
{
    const std::string prefix = "readonly ";
    const size_t prefixLen = prefix.length();

    if (str.compare(0, prefixLen, prefix) == 0)
    {
        str.erase(0, prefixLen);
    }
}
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
        // str += structType->GetName();
        str += structType->GetUID();
        break;
    }
    case DataType::Type::ENUM:
    {
        auto enumType = dynamic_cast<EnumDataType *>(this);
        str += enumType->GetUID();
        // str += enumType->name;
        break;
    }
    case DataType::Type::UNION:
    {
        auto unionType = dynamic_cast<UnionDataType *>(this);
        str += unionType->GetUID();
        // str += unionType->name;
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

void SemanticAnalyzer::ReplaceGenericTypes(
    std::unordered_map<std::string, DataTypeToken *> &dataTypesMap)
{
    for (auto dt : dataTypesMap)
    {
        auto dtToken = dt.second;
        Token *first = nullptr;
        Token *second = nullptr;
        Token *third = nullptr;
        for (size_t index = dtToken->tokens.startPos;
             index < dtToken->tokens.endPos; index++)
        {

            if (TokenMatch(dtToken->tokens.tokens[index]->GetType(),
                           TokenType::READONLY))
            {
                continue;
            }

            if (TokenMatch(dtToken->tokens.tokens[index]->GetType(),
                           TokenType::IDENTIFIER))
            {
                first = dtToken->tokens.tokens[index];
                index++;

                if (TokenMatch(dtToken->tokens.tokens[index]->GetType(),
                               TokenType::DOUBLE_COLON))
                {

                    second = dtToken->tokens.tokens[index];
                    index++;
                    if (TokenMatch(dtToken->tokens.tokens[index]->GetType(),
                                   TokenType::DOUBLE_COLON))
                    {
                        third = dtToken->tokens.tokens[index];
                        index++;
                    }
                }
                index--;
            }

            Token *typeToken = nullptr;
            if (third)
            {
                typeToken = third;
            }
            else if (second)
            {
                typeToken = second;
            }
            else
            {
                typeToken = first;
            }

            if (typeToken)
            {
                DataType *type = currentScope->GetDataType(dtToken);

                Symbol *symbol = nullptr;

                if (type->GetType() == DataType::Type::STRUCT)
                {
                    auto sType = dynamic_cast<StructDataType *>(type);
                    symbol = dynamic_cast<Symbol *>(sType);
                }
                else if (type->GetType() == DataType::Type::UNION)
                {
                    auto uType = dynamic_cast<UnionDataType *>(type);
                    symbol = dynamic_cast<Symbol *>(uType);
                }
                else if (type->GetType() == DataType::Type::ENUM)
                {
                    auto eType = dynamic_cast<EnumDataType *>(type);
                    symbol = dynamic_cast<Symbol *>(eType);
                }
                assert(symbol);

                auto oldToken = dtToken->tokens.tokens[index];
                auto newToken =
                    gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                *newToken = *(oldToken);
                newToken->lexeme = symbol->uid;
                dtToken->tokens.tokens[index] = newToken;
                index++;
                // check for <>><><><
                if (dtToken->tokens.tokens[index]->GetType() ==
                    TokenType::LEFT_ANGLE_SQUARE)
                {
                    dtToken->tokens.tokens.erase(
                        dtToken->tokens.tokens.begin() + (int64_t)index);
                    dtToken->tokens.endPos--;
                    size_t layer = 1;
                    while (layer != 0)
                    {
                        if (dtToken->tokens.tokens[index]->GetType() ==
                            TokenType::LEFT_ANGLE_SQUARE)
                        {
                            layer++;
                        }
                        else if (dtToken->tokens.tokens[index]->GetType() ==
                                 TokenType::RIGHT_SQUARE_ANGLE)
                        {
                            layer--;
                        }
                        dtToken->tokens.tokens.erase(
                            dtToken->tokens.tokens.begin() + (int64_t)index);
                        dtToken->tokens.endPos--;
                    }
                }
            }

            break;
        }
    }
}
void SemanticAnalyzer::ReplaceGenericTypesInStatement(
    Tokens &tokens, Generic *generic, std::vector<Token *> &processTokens,
    std::unordered_map<std::string, DataTypeToken *> &dataTypesMap)
{
    long long i = 0;
    size_t layer = 0;

    // Replace all generic types with provided types
    while (i != tokens.endPos)
    {
        auto itr = std::find_if(
            generic->types.begin(), generic->types.end(),
            [&](DataTypeToken *t) -> bool
            { return t->ToString() == processTokens[i]->GetLexeme(); });
        if (itr != generic->types.end())
        {
            auto dtToken = dataTypesMap[processTokens[i]->GetLexeme()];
            bool first = true;
            for (size_t index = dtToken->tokens.startPos;
                 index < dtToken->tokens.endPos; index++)
            {
                auto oldToken = processTokens[i];
                auto newToken =
                    gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                *newToken = *(oldToken);
                newToken->type = dtToken->tokens.tokens[index]->GetType();
                newToken->lexeme = dtToken->tokens.tokens[index]->lexeme;
                if (first)
                {
                    first = false;
                    processTokens[i] = newToken;
                }
                else
                {
                    processTokens.insert(
                        processTokens.begin() + (long long)i + 1, newToken);
                    i++;
                    tokens.endPos++;
                }
            }
        }
        i++;
    }
}
DataType *Scope::GetDataType(DataTypeToken *dataTypeToken)
{
    std::string typeStr = dataTypeToken->ToString();

    if (semanticAnalyzer->uidToSymbolMap.contains(typeStr))
    {
        auto symbol = semanticAnalyzer->uidToSymbolMap[typeStr];
        DataType *res = nullptr;
        if (dynamic_cast<StructDataType *>(symbol))
        {
            res = dynamic_cast<DataType *>(
                dynamic_cast<StructDataType *>(symbol));
        }
        else if (dynamic_cast<UnionDataType *>(symbol))
        {
            res =
                dynamic_cast<DataType *>(dynamic_cast<UnionDataType *>(symbol));
        }
        else if (dynamic_cast<EnumDataType *>(symbol))
        {
            res =
                dynamic_cast<DataType *>(dynamic_cast<EnumDataType *>(symbol));
        }
        assert(res);
        return res;
    }

    DataType *result = nullptr;
    if (typeStr.empty())
    {
        typeStr = "notype";
    }

    Scope *currentScope = this;

    if ((dataTypeToken->libToken && dataTypeToken->pkgToken))
    {
        Library *libFound = nullptr;
        for (auto lib : semanticAnalyzer->libraries)
        {
            if (lib->name == dataTypeToken->libToken->GetLexeme())
            {
                libFound = lib;
                break;
            }
        }

        if (!libFound)
        {
            CodeErrString ces;
            ces.firstToken = dataTypeToken->libToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("Library '{}' not found.",
                                    dataTypeToken->libToken->GetLexeme()),
                        ces);
        }

        Package *pkgFound = nullptr;
        std::stack<Library *> libStack;
        libStack.push(libFound);
        while (!libStack.empty())
        {
            auto topLib = libStack.top();
            libStack.pop();
            for (auto l : topLib->libs)
            {
                libStack.push(l);
            }

            for (auto pkg : topLib->packages)
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

        if (!semanticAnalyzer->pkgToScopeMap.contains(pkgFound))
        {
            assert(0);
        }

        currentScope = semanticAnalyzer->pkgToScopeMap[pkgFound];
    }
    else if (dataTypeToken->pkgToken)
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
            Scope *scope = semanticAnalyzer->currentScope;
            while (scope)
            {
                for (auto lib : scope->importedLibs)
                {
                    std::stack<Library *> libStack;
                    libStack.push(lib);
                    while (!libStack.empty())
                    {
                        auto topLib = libStack.top();
                        libStack.pop();
                        for (auto l : topLib->libs)
                        {
                            libStack.push(l);
                        }

                        for (auto pkg : topLib->packages)
                        {
                            if (pkg->GetIdentifier()->GetLexeme() ==
                                dataTypeToken->pkgToken->GetLexeme())
                            {
                                pkgFound = pkg;
                                break;
                            }
                        }
                    }
                }
                scope = scope->parent;
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
        }

        if (!semanticAnalyzer->pkgToScopeMap.contains(pkgFound))
        {
            assert(0);
        }

        currentScope = semanticAnalyzer->pkgToScopeMap[pkgFound];
    }

    if (!dataTypeToken->arrayDesc)
    {
        while (currentScope && !result)
        {
            if (currentScope->datatypesMap.contains(typeStr))
            {
                DataType *type = currentScope->datatypesMap[typeStr];

                result = type;
                break;
            }
            for (auto pkg : currentScope->importedPackages)
            {
                if (pkg->datatypesMap.contains(typeStr))
                {

                    DataType *type = pkg->datatypesMap[typeStr];

                    auto symbolType = dynamic_cast<Symbol *>(type);

                    if (symbolType)
                    {
                        if (!symbolType->IsPublic())
                        {
                            ReportError(std::format("'{}' is private",
                                                    symbolType->GetName()),
                                        dataTypeToken->GetCodeErrString());
                        }
                    }

                    result = type;
                    break;
                }
            }
            currentScope = currentScope->parent;
        }
    }

    if (result && dataTypeToken->generic)
    {
        auto structType = dynamic_cast<StructDataType *>(result);
        auto unionType = dynamic_cast<UnionDataType *>(result);
        if (structType || unionType)
        {
            Generic *generic = nullptr;
            Tokens tokens;
            std::string genericTypeName;
            if (structType)
            {
                generic = structType->structStmt->generic;
                tokens = structType->structStmt->tokens;
                genericTypeName = structType->GetName();
            }
            else if (unionType)
            {
                generic = unionType->unionStmt->generic;
                tokens = unionType->unionStmt->tokens;
                genericTypeName = unionType->GetName();
            }

            if (!generic)
            {
                ReportError(
                    std::format("Type '{}' is not generic", genericTypeName),
                    dataTypeToken->GetCodeErrString());
            }

            if (dataTypeToken->generic->types.size() != generic->types.size())
            {
                ReportError(
                    std::format("Wrong number of types provided to generic "
                                "type '{}'",
                                genericTypeName),
                    dataTypeToken->GetCodeErrString());
            }

            currentScope = dataTypeToken->pkgToken
                               ? currentScope
                               : semanticAnalyzer->currentScope;
            std::vector<DataType *> gTypes;
            for (auto t : dataTypeToken->generic->types)
            {
                gTypes.push_back(currentScope->GetDataType(t));
            }

            std::function<std::string(std::string, std::vector<DataType *> &)>
                genName = nullptr;
            genName = [](std::string typeName,
                         std::vector<DataType *> &gTypes) -> std::string
            {
                std::string name = typeName;
                for (auto type : gTypes)
                {
                    name += std::format("_{}", type->ToString());
                }
                return name;
            };

            genericTypeName = genName(genericTypeName, gTypes);
            auto foundSymbol = currentScope->GetSymbol(
                genericTypeName, dataTypeToken->GetCodeErrString(), true);
            if (foundSymbol)
            {
                if (structType)
                {
                    result = dynamic_cast<StructDataType *>(foundSymbol);
                }
                else if (unionType)
                {
                    result = dynamic_cast<UnionDataType *>(foundSymbol);
                }
            }
            else
            {
                std::unordered_map<std::string, DataTypeToken *> dataTypesMap;

                for (size_t i = 0; i < dataTypeToken->generic->types.size();
                     i++)
                {
                    dataTypesMap[generic->types[i]->ToString()] =
                        dataTypeToken->generic->types[i];
                }

                // remove generics tokens
                std::vector<Token *> processTokens = tokens.tokens;

                // set scope
                auto tmp = semanticAnalyzer->currentScope;
                semanticAnalyzer->currentScope = currentScope;
                semanticAnalyzer->ReplaceGenericTypes(dataTypesMap);
                semanticAnalyzer->currentScope = tmp;
                semanticAnalyzer->ReplaceGenericTypesInStatement(
                    tokens, generic, processTokens, dataTypesMap);

                bool renamed = false;
                for (auto itr =
                         processTokens.begin() + (long long)tokens.startPos;
                     itr != processTokens.begin() + (long long)tokens.endPos;
                     itr++)
                {
                    if ((*itr)->GetType() == TokenType::IDENTIFIER && !renamed)
                    {
                        renamed = true;
                        auto oldID = *itr;
                        *itr =
                            gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                        *(*itr) = *oldID;
                        (*itr)->lexeme = genericTypeName;
                    }
                    if ((*itr)->GetType() == TokenType::LEFT_ANGLE_SQUARE)
                    {
                        size_t layer = 1;
                        itr = processTokens.erase(itr);
                        while (layer != 0)
                        {
                            if ((*itr)->GetType() ==
                                TokenType::LEFT_ANGLE_SQUARE)
                            {
                                layer++;
                            }
                            else if ((*itr)->GetType() ==
                                     TokenType::RIGHT_SQUARE_ANGLE)
                            {
                                layer--;
                            }

                            itr = processTokens.erase(itr);
                        }
                        break;
                    }
                }

                Parser parser(processTokens);
                parser.currentIndex = tokens.startPos;
                Statement *newStatement = nullptr;
                if (structType)
                {
                    StructStatement *newStructStatement =
                        dynamic_cast<StructStatement *>(
                            parser.ParseStructStatement());
                    if (!newStructStatement)
                    {
                        ReportError("Failed to generate generic type",
                                    dataTypeToken->GetCodeErrString());
                    }

                    newStatement = newStructStatement;
                }
                else if (unionType)
                {
                    UnionStatement *newUnionStatement =
                        dynamic_cast<UnionStatement *>(
                            parser.ParseStructStatement());
                    if (!newUnionStatement)
                    {
                        ReportError("Failed to generate generic type",
                                    dataTypeToken->GetCodeErrString());
                    }
                    newStatement = newUnionStatement;
                }

                GenericStatementInfo temp = {};

                temp.currentScope = semanticAnalyzer->currentScope;
                temp.currentBlockLeaf = semanticAnalyzer->currentBlockLeaf;
                temp.currentFunction = semanticAnalyzer->currentFunction;
                temp.currentLibrary = semanticAnalyzer->currentLibrary;
                temp.currentPackage = semanticAnalyzer->currentPackage;

                GenericStatementInfo *genericInfo = nullptr;
                Statement *genericStatement = nullptr;
                if (structType)
                {
                    genericStatement = structType->structStmt;
                    genericInfo = structType->generic;
                }
                else if (unionType)
                {
                    genericStatement = unionType->unionStmt;
                    genericInfo = unionType->generic;
                }
                semanticAnalyzer->currentScope = genericInfo->currentScope;
                semanticAnalyzer->currentBlockLeaf =
                    genericInfo->currentBlockLeaf;
                semanticAnalyzer->currentFunction =
                    genericInfo->currentFunction;
                semanticAnalyzer->currentLibrary = genericInfo->currentLibrary;
                semanticAnalyzer->currentPackage = genericInfo->currentPackage;
                if (structType)
                {
                    semanticAnalyzer->AnalyzeStructStatement(
                        dynamic_cast<StructStatement *>(newStatement),
                        semanticAnalyzer->currentFunction, true, false, false);
                    semanticAnalyzer->AnalyzeStructStatement(
                        dynamic_cast<StructStatement *>(newStatement),
                        semanticAnalyzer->currentFunction, false, true, false);
                    semanticAnalyzer->AnalyzeStructStatement(
                        dynamic_cast<StructStatement *>(newStatement),
                        semanticAnalyzer->currentFunction, false, false, true);
                }
                else if (unionType)
                {
                    semanticAnalyzer->AnalyzeUnionStatement(
                        dynamic_cast<UnionStatement *>(newStatement),
                        semanticAnalyzer->currentFunction, true, false);
                    semanticAnalyzer->AnalyzeUnionStatement(
                        dynamic_cast<UnionStatement *>(newStatement),
                        semanticAnalyzer->currentFunction, false, true);
                }

                if (auto blockLeaf = genericInfo->currentBlockLeaf)
                {
                    auto itr =
                        std::find(blockLeaf->blockStmt->GetStatements().begin(),
                                  blockLeaf->blockStmt->GetStatements().end(),
                                  genericStatement);

                    if (itr == blockLeaf->blockStmt->GetStatements().end())
                    {
                        PrintError("Internal error: generic statement "
                                   "cannot be found");
                    }
                    long long index =
                        itr - blockLeaf->blockStmt->GetStatements().begin();
                    semanticAnalyzer->stmtsToAdd.push_back(
                        {blockLeaf->blockStmt->GetStatements(), index,
                         newStatement});
                }
                else
                {
                    auto itr = std::find(
                        semanticAnalyzer->currentPackage->GetStatements()
                            .begin(),
                        semanticAnalyzer->currentPackage->GetStatements().end(),
                        genericStatement);
                    if (itr ==
                        semanticAnalyzer->currentPackage->GetStatements().end())
                    {
                        PrintError("Internal error: generic statement "
                                   "cannot be found");
                    }
                    long long index =
                        itr - semanticAnalyzer->currentPackage->GetStatements()
                                  .begin();
                    semanticAnalyzer->stmtsToAdd.push_back(
                        {semanticAnalyzer->currentPackage->GetStatements(),
                         index, newStatement});
                }

                semanticAnalyzer->currentScope = temp.currentScope;
                semanticAnalyzer->currentBlockLeaf = temp.currentBlockLeaf;
                semanticAnalyzer->currentFunction = temp.currentFunction;
                semanticAnalyzer->currentLibrary = temp.currentLibrary;
                semanticAnalyzer->currentPackage = temp.currentPackage;

                result = semanticAnalyzer->stmtToDataTypeMap[newStatement];
            }
        }
    }

    else if (dataTypeToken->fnStatement)
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

    else if (dataTypeToken->arrayDesc)
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
    else if (dataTypeToken->pointerDesc)
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

    if (!result)
    {
        CodeErrString ces;
        ces.firstToken = dataTypeToken->GetFirstToken();
        ces.str = ces.firstToken->GetLexeme();
        ReportError("Unkown type", ces);
    }

    return result;
}

Scope::Scope(SemanticAnalyzer *semanticAnalyzer, std::string name,
             SourceLocation location, Scope *parent)
    : location(location)
{
    if (parent)
    {
        this->name = std::format("{}::{}", parent->name, name);
    }
    else
    {
        this->name = name;
    }
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
    case Type::POINTER:
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

Symbol *Scope::GetSymbol(std::string name, CodeErrString codeErrString,
                         bool check)
{
    Symbol *symbol = nullptr;
    Scope const *current = this;

    if (semanticAnalyzer->uidToSymbolMap.contains(name))
    {
        return semanticAnalyzer->uidToSymbolMap[name];
    }

    while (!symbol)
    {
        std::string _name = std::format("{}::{}", current->name, name);
        if (current->symbolsMap.contains(_name))
        {
            symbol = current->symbolsMap.at(_name);
            break;
        }

        for (auto pkgScope : current->importedPackages)
        {
            _name = std::format("{}::{}", pkgScope->name, name);
            if (pkgScope->symbolsMap.contains(_name))
            {
                symbol = pkgScope->symbolsMap.at(_name);
                if (!symbol->IsPublic())
                {
                    ReportError(std::format("'{}' is private member.", name),
                                codeErrString);
                }
                break;
            }
        }
        for (auto lib : importedLibs)
        {
            std::stack<Library *> libStack;
            libStack.push(lib);
            while (!libStack.empty())
            {
                auto topLib = libStack.top();
                libStack.pop();
                for (auto l : topLib->libs)
                {
                    libStack.push(l);
                }

                for (auto pkg : topLib->packages)
                {
                    auto pkgScope = semanticAnalyzer->pkgToScopeMap[pkg];
                    _name = std::format("{}::{}", pkgScope->name, name);
                    if (pkgScope->symbolsMap.contains(_name))
                    {
                        symbol = pkgScope->symbolsMap.at(_name);
                        if (!symbol->IsPublic())
                        {
                            ReportError(
                                std::format("'{}' is private member.", name),
                                codeErrString);
                        }
                        break;
                    }
                }
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
    else if (!check)
    {
        ReportError(std::format("'{}' is not defined in this scope.", name),
                    codeErrString);
    }
    return nullptr;
}
void Scope::AddSymbol(Symbol *symbol, CodeErrString codeErrString)
{
    std::string symbolName = std::format("{}::{}", name, symbol->GetName());
    if (symbolsMap.contains(symbolName))
    {
        Symbol *alreadyDefined = symbolsMap[symbolName];
        ReportError(std::format("'{}' already defined", symbol->GetName()),
                    codeErrString);
    }
    else
    {
        symbolsMap[symbolName] = symbol;
        symbol->uid = std::format("{}_{}", symbol->GetName(), (size_t)symbol);
        semanticAnalyzer->uidToSymbolMap[symbol->uid] = symbol;
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
        std::string r = rValueArrType->ToString();
        std::string l = arrType->ToString();
        removeReadonlyPrefix(r);
        removeReadonlyPrefix(l);
        if (r != l)
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
    for (auto l : libraries)
    {
        for (auto pkg : l->packages)
        {
            AnalyzePackage(pkg, true, false, false, false);
        }
    }

    for (auto pkg : packages)
    {
        AnalyzePackage(pkg, true, false, false, false);
    }

    for (auto l : libraries)
    {
        for (auto pkg : l->packages)
        {
            AnalyzePackage(pkg, false, true, false, false);
        }
    }
    for (auto pkg : packages)
    {
        AnalyzePackage(pkg, false, true, false, false);
    }
    for (auto l : libraries)
    {
        for (auto pkg : l->packages)
        {
            AnalyzePackage(pkg, false, false, true, false);
        }
    }
    for (auto pkg : packages)
    {
        AnalyzePackage(pkg, false, false, true, false);
    }
    for (auto l : libraries)
    {
        for (auto pkg : l->packages)
        {
            AnalyzePackage(pkg, false, false, false, true);
        }
    }
    for (auto pkg : packages)
    {
        AnalyzePackage(pkg, false, false, false, true);
    }
    for (auto toAdd : stmtsToAdd)
    {
        toAdd.statements.insert(toAdd.statements.begin() + toAdd.index,
                                toAdd.stmt);
    }
}
SemanticAnalyzer::~SemanticAnalyzer() {}
void SemanticAnalyzer::AnalyzePackage(Package *pkg, bool analyzeTypes,
                                      bool analyzeFnVar, bool analyzeTypeBody,
                                      bool analyzeFnVarBody)
{
    if (pkgToScopeMap.contains(pkg))
    {
        currentPackage = pkg;
        currentScope = pkgToScopeMap[pkg];
    }
    if (analyzeTypes)
    {

        PackageDataType *pkgType = gZtoonArena.Allocate<PackageDataType>();

        pkgToDataTypeMap[pkg] = pkgType;

        pkgType->type = PackageDataType::Type::PACKAGE;
        pkgType->name = pkg->identifier->GetLexeme();
        pkgType->pkg = pkg;

        currentScope = gZtoonArena.Allocate<Scope>(
            this,
            std::format("{}_{}", pkg->GetIdentifier()->GetLexeme(),
                        (size_t)pkg),
            (SourceLocation){pkg->GetIdentifier()->GetFilepath(),
                             (uint32_t)pkg->GetIdentifier()->GetLineNumber(),
                             (uint32_t)pkg->GetIdentifier()->GetColNumber()});
        pkgToScopeMap[pkg] = currentScope;

        CodeErrString ces;
        ces.firstToken = pkg->identifier;
        ces.str = ces.firstToken->GetLexeme();
        pkgType->isPublic = true;
        currentScope->AddSymbol(pkgType, ces);
        currentPackage = pkg;
        AnalyzePackageGlobalTypes(pkg);
    }
    if (analyzeFnVar)
    {
        for (auto stmt : pkg->GetStatements())
        {
            if (dynamic_cast<ImportStatement *>(stmt))
            {
                auto importStmt = dynamic_cast<ImportStatement *>(stmt);
                AnalyzeImportStatement(importStmt);
            }
        }
        AnalyzePackageGlobalFuncsAndVars(pkg);
    }
    if (analyzeTypeBody)
        AnalyzePackageGlobalTypeBodies(pkg);
    if (analyzeFnVarBody)
        AnalyzePackageVarAndFuncBodies(pkg);
}
void SemanticAnalyzer::Analyze(std::vector<Package *> &packages) {}

void SemanticAnalyzer::AnalyzePackageGlobalTypes(Package *pkg)
{
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<StructStatement *>(stmt))
        {
            auto structStmt = dynamic_cast<StructStatement *>(stmt);
            AnalyzeStructStatement(structStmt, true, true, false, false);
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
            AnalyzeStructStatement(structStmt, true, false, true, false);
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
    for (auto stmt : pkg->statements)
    {
        if (dynamic_cast<StructStatement *>(stmt))
        {
            auto structStmt = dynamic_cast<StructStatement *>(stmt);
            AnalyzeStructStatement(structStmt, true, false, false, true);
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
        blockStatement->firstToken =
            dynamic_cast<ForLoopStatement *>(statement)->forToken;
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
    else if (dynamic_cast<DeferStatement *>(statement))
    {
        auto deferStmt = dynamic_cast<DeferStatement *>(statement);
        AnalyzeDeferStatement(deferStmt);
    }
    else if (dynamic_cast<StructStatement *>(statement))
    {
        auto structStmt = dynamic_cast<StructStatement *>(statement);
        AnalyzeStructStatement(structStmt, false, true, true, true);
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
        scope = gZtoonArena.Allocate<Scope>(
            this, std::format("block_statement_{}", (size_t)blockStmt),
            (SourceLocation){
                blockStmt->GetFirstToken()->GetFilepath(),
                (uint32_t)blockStmt->GetFirstToken()->GetLineNumber(),
                (uint32_t)blockStmt->GetFirstToken()->GetColNumber()},
            currentScope);
        blockToScopeMap[blockStmt] = scope;
    }
    else
    {
        scope = blockToScopeMap[blockStmt];
    }

    Scope *temp = currentScope;

    currentScope = scope;

    BlockStatementLeaf *tempBlock = currentBlockLeaf;

    BlockStatementLeaf *newBlock = gZtoonArena.Allocate<BlockStatementLeaf>();

    newBlock->blockStmt = blockStmt;
    newBlock->parent = currentBlockLeaf;

    currentBlockLeaf = newBlock;

    currentBlockLeaf->blockStmt = blockStmt;

    bool doneDefer = false;
    std::vector<Statement *> rets;
    // for ret stmts need to go up tree
    // for break and continue, no need to go up tree.
    for (size_t i = 0; i < blockStmt->statements.size(); i++)
    {
        auto retStmt = dynamic_cast<RetStatement *>(blockStmt->statements[i]);
        auto contStmt =
            dynamic_cast<ContinueStatement *>(blockStmt->statements[i]);
        auto breakStmt =
            dynamic_cast<BreakStatement *>(blockStmt->statements[i]);
        if (retStmt || contStmt || breakStmt)
        {
            bool retStmtFound =
                std::find(rets.begin(), rets.end(),
                          dynamic_cast<RetStatement *>(
                              blockStmt->statements[i])) != rets.end();
            bool continueStmtFound =
                std::find(rets.begin(), rets.end(),
                          dynamic_cast<ContinueStatement *>(
                              blockStmt->statements[i])) != rets.end();
            bool breakStmtFound =
                std::find(rets.begin(), rets.end(),
                          dynamic_cast<BreakStatement *>(
                              blockStmt->statements[i])) != rets.end();
            if (!retStmtFound && !continueStmtFound && !breakStmtFound)
            {

                BlockStatementLeaf *currentLeaf = currentBlockLeaf;
                while (currentLeaf)
                {
                    rets.push_back(
                        dynamic_cast<RetStatement *>(blockStmt->statements[i]));
                    if (deferredStatementsMap.contains(currentLeaf))
                    {
                        doneDefer = true;

                        for (int64_t index =
                                 (int64_t)deferredStatementsMap[currentLeaf]
                                     .size() -
                                 1;
                             index >= 0; index--)
                        {
                            auto stmt =
                                deferredStatementsMap[currentLeaf][index];
                            BlockStatement *newBlock =
                                gZtoonArena.Allocate<BlockStatement>();
                            newBlock->firstToken = blockStmt->firstToken;
                            newBlock->GetStatements().push_back(stmt);
                            blockStmt->statements.insert(
                                blockStmt->statements.begin() + (int64_t)i,
                                newBlock);
                        }
                    }

                    currentLeaf = currentLeaf->parent;

                    if ((contStmt || breakStmt))
                    {
                        if (currentLeaf->blockStmt->loopBlock)
                        {
                            currentLeaf = nullptr;
                        }
                    }
                }
            }
        }

        AnalyzeStatement(blockStmt->statements[i]);
        blockStmt->index = i;
    }

    if (!doneDefer)
    {
        if (deferredStatementsMap.contains(currentBlockLeaf))
        {
            doneDefer = true;
            for (int64_t index =
                     (int64_t)deferredStatementsMap[currentBlockLeaf].size() -
                     1;
                 index >= 0; index--)
            {
                auto stmt = deferredStatementsMap[currentBlockLeaf][index];
                AnalyzeStatement(stmt);
                blockStmt->statements.push_back(stmt);
            }
        }
    }

    currentScope = temp;
    currentBlockLeaf = tempBlock;
}

void SemanticAnalyzer::AnalyzeFnStatement(FnStatement *fnStmt, bool isGlobal,
                                          bool analyzeSymbol, bool analyzeBody,
                                          bool isMethod)
{
    if (analyzeSymbol)
    {
        Function *fp = gZtoonArena.Allocate<Function>();
        fp->fnStmt = fnStmt;
        std::string fpName = fnStmt->GetIdentifier()->GetLexeme();
        fp->name = fpName;
        fp->isMethod = fnStmt->method;
        currentScope->AddSymbol(fp, fnStmt->GetCodeErrString());
        if (fnStmt->generic)
        {
            FnDataType *fpDataType = gZtoonArena.Allocate<FnDataType>();
            fpDataType->type = DataType::Type::FN;
            fpDataType->isVarArgs = fnStmt->IsVarArgs();
            fpDataType->isMethod = fnStmt->method;
            fpDataType->fn = fp;

            fp->generic = gZtoonArena.Allocate<GenericStatementInfo>();
            fp->generic->currentBlockLeaf = currentBlockLeaf;
            fp->generic->currentFunction = currentFunction;
            fp->generic->currentLibrary = currentLibrary;
            fp->generic->currentPackage = currentPackage;
            fp->generic->currentScope = currentScope;
            fp->fnPointer = fpDataType->GetFnPtrType();
            fp->isPublic = fnStmt->IsPublic();
        }
        else
        {
            FnDataType *fpDataType = gZtoonArena.Allocate<FnDataType>();
            fpDataType->type = DataType::Type::FN;
            fp->isPublic = fnStmt->IsPublic();
            fpDataType->isVarArgs = fnStmt->IsVarArgs();
            fpDataType->isMethod = fnStmt->method;
            fpDataType->fn = fp;
            fp->fnPointer = fpDataType->GetFnPtrType();
            fpDataType->returnDataType =
                currentScope->GetDataType(fnStmt->returnDataTypeToken);

            Function *temp = currentFunction;
            currentFunction = fp;

            auto tempScope = currentScope;
            currentScope = gZtoonArena.Allocate<Scope>(
                this,
                fnStmt->IsPrototype()
                    ? ""
                    : std::format("{}_{}", fnStmt->GetIdentifier()->GetLexeme(),
                                  (size_t)fnStmt),
                (SourceLocation){
                    fnStmt->GetIdentifier()->GetFilepath(),
                    (uint32_t)fnStmt->GetIdentifier()->GetLineNumber(),
                    (uint32_t)fnStmt->GetIdentifier()->GetColNumber()},
                nullptr);

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
            currentScope->datatypesMap[fp->fnPointer->ToString()] =
                fp->fnPointer;
            currentFunction = temp;
        }
    }
    if (analyzeBody && !fnStmt->generic)
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
        currentFunction = temp;
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
        var->isPublic = varDeclStmt->IsPublic();
        currentScope->AddSymbol(var, varDeclStmt->GetCodeErrString());
        if (currentFunction && !varDeclStmt->isField &&
            !varDeclStmt->IsParamter())
        {
            TopVarDecl topDecl;
            topDecl.varDecl = varDeclStmt;
            topDecl.currentScope = currentScope;
            fnToVarDeclsMap[currentFunction].push_back(topDecl);
        }
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
        auto lType = stmtToDataTypeMap[varAssignmentStmt];
        if (lType->IsReadOnly())
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
    if (switchStmt->GetCases().empty() && !switchStmt->defualtCase)
    {
        ReportError("Cannot have empty switch statement",
                    switchStmt->GetCodeErrString());
    }
    if (switchStmt->GetCases().empty() && switchStmt->defualtCase)
    {
        ReportError("Cannot have only default case",
                    switchStmt->GetCodeErrString());
    }
    AnalyzeExpression(switchStmt->matchExpr);
    IfStatement *ifStmt = gZtoonArena.Allocate<IfStatement>();
    bool first = true;

    auto tokenEQEQ = gZtoonArena.Allocate<Token>(TokenType::EQUAL_EQUAL);
    *tokenEQEQ = *switchStmt->token;
    tokenEQEQ->type = TokenType::EQUAL_EQUAL;
    tokenEQEQ->lexeme = "==";
    auto tokenOR = gZtoonArena.Allocate<Token>(TokenType::OR);
    tokenOR->type = TokenType::OR;
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
    for (size_t index = 0;
         index < currentBlockLeaf->blockStmt->statements.size(); index++)
    {
        if (currentBlockLeaf->blockStmt->statements[index] == switchStmt)
        {
            currentBlockLeaf->blockStmt->statements[index] = ifStmt;
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
    currentBlockLeaf->blockStmt->loopBlock = true;
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
    currentBlockLeaf->blockStmt->loopBlock = true;
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
                                              bool analyzeBody,
                                              bool analyzeMethodsBody)
{
    if (analyzeSymbol)
    {
        if (currentFunction)
        {
            TopAggTypeDecl aggType;
            aggType.currentScope = currentScope;
            aggType.structStmt = structStmt;
            fnToAggDeclsMap[currentFunction].push_back(aggType);
        }

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
            if (!structStmt->unions.empty() || !structStmt->methods.empty())
            {
                ReportError("Anonymous structs can only have variable fields.",
                            structStmt->GetCodeErrString());
            }
            structType->name = std::format("__anonymous_struct__number__{}",
                                           (size_t)(structStmt));
        }
        if (structStmt->identifier && structStmt->generic)
        {
            structType->generic = gZtoonArena.Allocate<GenericStatementInfo>();
            structType->generic->currentBlockLeaf = currentBlockLeaf;
            structType->generic->currentFunction = currentFunction;
            structType->generic->currentLibrary = currentLibrary;
            structType->generic->currentPackage = currentPackage;
            structType->generic->currentScope = currentScope;
        }

        currentScope->datatypesMap[structType->name] = structType;
        structType->isPublic = structStmt->IsPublic();
        currentScope->AddSymbol(structType, structStmt->GetCodeErrString());
        stmtToDataTypeMap[structStmt] = structType;
        structType->fullName =
            std::format("{}::{}", currentScope->name, structType->name);
        Scope *scope = gZtoonArena.Allocate<Scope>(
            this,
            std::format("{}_{}", structType->GetName(), (size_t)structType),
            (SourceLocation){
                structStmt->GetIDToken()->GetFilepath(),
                (uint32_t)structStmt->GetIDToken()->GetLineNumber(),
                (uint32_t)structStmt->GetIDToken()->GetColNumber()},
            currentScope);
        structType->scope = scope;
    }
    if (analyzeBody && !structStmt->generic)
    {
        auto structType =
            dynamic_cast<StructDataType *>(stmtToDataTypeMap[structStmt]);
        Scope *temp = currentScope;
        currentScope = structType->scope;
        structType->defaultValuesList =
            gZtoonArena.Allocate<InitializerListExpression>();
        for (auto uField : structStmt->unions)
        {
            auto tempSScope = currentScope;
            AnalyzeUnionStatement(uField, false, true, true);
            currentScope = tempSScope;
            auto uType =
                dynamic_cast<UnionDataType *>(stmtToDataTypeMap[uField]);
            for (auto s : uType->scope->symbolsMap)
            {
                currentScope->AddSymbol(s.second,
                                        structStmt->GetCodeErrString());
            }
            VarDeclStatement *var = gZtoonArena.Allocate<VarDeclStatement>();
            var->isField = true;
            DataTypeToken *dataToken = gZtoonArena.Allocate<DataTypeToken>();
            auto dataTypeToken =
                gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            *dataTypeToken = *uField->GetIDToken();
            dataTypeToken->type = TokenType::IDENTIFIER;
            dataTypeToken->lexeme = uType->GetName();
            dataToken->dataType = dataTypeToken;
            var->dataTypeToken = dataToken;
            auto idToken = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            *idToken = *uField->GetIDToken();
            idToken->type = TokenType::IDENTIFIER;
            idToken->lexeme = uType->GetName() + "_field";
            var->identifier = idToken;

            // AnalyzeVarDeclStatement(var, true, true, false);
            long long loc = 0;
            for (long long i = 0; i < structStmt->fieldsInOrder.size(); i++)
            {
                if (structStmt->fieldsInOrder[i] == uField)
                {
                    loc = i;
                    break;
                }
            }

            structStmt->fields.insert(structStmt->fields.begin() + loc, var);
        }
        size_t index = 0;
        for (auto field : structStmt->fields)
        {
            AnalyzeStatement(field);
            auto fieldDataType = stmtToDataTypeMap[field];
            auto fieldStructType =
                dynamic_cast<StructDataType *>(fieldDataType);
            auto fieldUnionType = dynamic_cast<UnionDataType *>(fieldDataType);

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
                                stmtToDataTypeMap[f]);
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                stmtToDataTypeMap[f]);
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
                                stmtToDataTypeMap[f]);
                            auto fUnionType = dynamic_cast<UnionDataType *>(
                                stmtToDataTypeMap[f]);
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

            index++;
        }
        for (auto f : structStmt->fieldsInOrder)
        {
            auto fieldStructType =
                dynamic_cast<StructDataType *>(stmtToDataTypeMap[f]);
            auto field = dynamic_cast<VarDeclStatement *>(f);
            if (fieldStructType && field && !field->GetExpression())
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
            else if (field)
            {
                structType->defaultValuesList->expressions.push_back(
                    field->GetExpression());
            }
            else
            {

                structType->defaultValuesList->expressions.push_back(nullptr);
            }
            structType->fields.push_back(stmtToDataTypeMap[f]);
        }
        InitListType *listType = gZtoonArena.Allocate<InitListType>();
        listType->type = DataType::Type::InitList;

        listType->dataTypes = structType->fields;

        exprToDataTypeMap[structType->defaultValuesList] = listType;
        for (auto method : structStmt->methods)
        {

            AnalyzeFnStatement(method, false, true, false, true);
        }

        structType->complete = true;
        currentScope = temp;
    }
    if (analyzeMethodsBody && !structStmt->generic)
    {
        auto structType =
            dynamic_cast<StructDataType *>(stmtToDataTypeMap[structStmt]);
        Scope *temp = currentScope;
        currentScope = structType->scope;

        for (auto method : structStmt->methods)
        {
            FnStatement *fnStmt = method;
            AnalyzeFnStatement(method, false, false, true, true);
        }
        currentScope = temp;
    }
}
void SemanticAnalyzer::AnalyzeUnionStatement(UnionStatement *unionStmt,
                                             bool isGlobal, bool analyzeSymbol,
                                             bool analyzeBody)
{
    if (analyzeSymbol)
    {
        if (unionStmt->GetFields().empty())
        {
            ReportError("Union type cannot be empty",
                        unionStmt->GetCodeErrString());
        }

        if (currentFunction)
        {
            TopAggTypeDecl aggType;
            aggType.currentScope = currentScope;
            aggType.unionStmt = unionStmt;
            fnToAggDeclsMap[currentFunction].push_back(aggType);
        }
        UnionDataType *unionType = gZtoonArena.Allocate<UnionDataType>();
        unionType->unionStmt = unionStmt;
        unionType->type = DataType::Type::UNION;
        if (unionStmt->identifier)
        {
            unionType->name = unionStmt->identifier->GetLexeme();
        }
        else
        {
            unionType->name = std::format("__anonymous_union__number__{}",
                                          (size_t)(unionStmt));
        }
        unionType->fullName =
            std::format("{}::{}", currentScope->name, unionType->name);
        currentScope->datatypesMap[unionType->name] = unionType;
        unionType->isPublic = unionStmt->IsPublic();
        currentScope->AddSymbol(unionType, unionStmt->GetCodeErrString());
        stmtToDataTypeMap[unionStmt] = unionType;
        if (unionStmt->identifier && unionStmt->generic)
        {
            unionType->generic = gZtoonArena.Allocate<GenericStatementInfo>();
            unionType->generic->currentBlockLeaf = currentBlockLeaf;
            unionType->generic->currentFunction = currentFunction;
            unionType->generic->currentLibrary = currentLibrary;
            unionType->generic->currentPackage = currentPackage;
            unionType->generic->currentScope = currentScope;
        }
    }
    if (analyzeBody && !unionStmt->generic)
    {
        auto unionType =
            dynamic_cast<UnionDataType *>(stmtToDataTypeMap[unionStmt]);

        Scope *scope = gZtoonArena.Allocate<Scope>(
            this, std::format("{}_{}", unionType->GetName(), (size_t)unionType),
            (SourceLocation){unionStmt->GetIDToken()->GetFilepath(),
                             (uint32_t)unionStmt->GetIDToken()->GetLineNumber(),
                             (uint32_t)unionStmt->GetIDToken()->GetColNumber()},
            currentScope);
        Scope *temp = currentScope;
        currentScope = scope;
        unionType->scope = currentScope;

        for (auto sField : unionStmt->structs)
        {
            auto tempUScope = currentScope;
            AnalyzeStructStatement(sField, false, true, true, false);
            currentScope = tempUScope;
            auto sType =
                dynamic_cast<StructDataType *>(stmtToDataTypeMap[sField]);
            for (auto s : sType->scope->symbolsMap)
            {
                currentScope->AddSymbol(s.second, sField->GetCodeErrString());
            }
            VarDeclStatement *var = gZtoonArena.Allocate<VarDeclStatement>();
            var->isField = true;
            DataTypeToken *dataToken = gZtoonArena.Allocate<DataTypeToken>();
            auto dataTypeToken =
                gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            *dataTypeToken = *sField->GetIDToken();
            dataTypeToken->type = TokenType::IDENTIFIER;
            dataTypeToken->lexeme = sType->GetName();
            dataToken->dataType = dataTypeToken;
            var->dataTypeToken = dataToken;
            auto idToken = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
            *idToken = *sField->GetIDToken();
            idToken->type = TokenType::IDENTIFIER;
            idToken->lexeme = sType->GetName() + "_field";
            var->identifier = idToken;
            long long loc = 0;
            for (long long i = 0; i < unionStmt->fieldsInOrder.size(); i++)
            {
                if (unionStmt->fieldsInOrder[i] == sField)
                {
                    loc = i;
                    break;
                }
            }

            unionStmt->fields.insert(unionStmt->fields.begin() + loc, var);
        }

        for (auto itr = unionStmt->GetFields().begin();
             itr != unionStmt->GetFields().end(); itr++)
        {
            auto field = *itr;
            AnalyzeStatement(field);
            StructStatement *fieldStructStmt =
                dynamic_cast<StructStatement *>(field);

            auto fieldDataType = stmtToDataTypeMap[field];
            auto fieldUnionType = dynamic_cast<UnionDataType *>(fieldDataType);
            auto fieldStructType =
                dynamic_cast<StructDataType *>(fieldDataType);
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
        currentScope = temp;
    }
}
void SemanticAnalyzer::AnalyzeEnumStatement(EnumStatement *enumStmt,
                                            bool isGlobal, bool analyzeSymbol,
                                            bool analyzeBody)
{
    if (analyzeSymbol)
    {
        if (currentFunction)
        {
            TopAggTypeDecl aggType;
            aggType.currentScope = currentScope;
            aggType.enumStmt = enumStmt;
            fnToAggDeclsMap[currentFunction].push_back(aggType);
        }
        EnumDataType *enumType = gZtoonArena.Allocate<EnumDataType>();
        enumType->type = DataType::Type::ENUM;
        enumType->name = enumStmt->identifier->GetLexeme();
        enumType->enumStmt = enumStmt;
        enumType->datatype = currentScope->GetDataType(enumStmt->datatype);
        enumType->isPublic = enumStmt->IsPublic();
        enumType->scope = gZtoonArena.Allocate<Scope>(
            this, std::format("{}_{}", enumType->GetName(), (size_t)enumType),
            (SourceLocation){enumStmt->identifier->GetFilepath(),
                             (uint32_t)enumStmt->identifier->GetLineNumber(),
                             (uint32_t)enumStmt->identifier->GetColNumber()},
            currentScope);
        if (!enumType->datatype->IsInteger())
        {
            ReportError("Enum type must be interger",
                        enumStmt->GetCodeErrString());
        }

        enumType->fullName =
            std::format("{}::{}", currentScope->name, enumType->name);
        stmtToDataTypeMap[enumStmt] = enumType;

        currentScope->datatypesMap[enumType->name] = enumType;
        currentScope->AddSymbol(enumType, enumStmt->GetCodeErrString());
    }
    if (analyzeBody)
    {
        auto enumType =
            dynamic_cast<EnumDataType *>(stmtToDataTypeMap[enumStmt]);
        auto temp = currentScope;
        currentScope = enumType->scope;
        for (auto f : enumStmt->fields)
        {
            ReadonlySymbol *readonlySymbol =
                gZtoonArena.Allocate<ReadonlySymbol>(
                    f->identifier->GetLexeme());
            readonlySymbol->dataType = enumType->GetDataType();
            readonlySymbol->token = f->identifier;
            currentScope->AddSymbol(readonlySymbol,
                                    enumStmt->GetCodeErrString());
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
        currentScope = temp;
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

        auto pkgScope = pkgToScopeMap[pkg];
        if (!pkgScope)
        {
            PrintMSG("Internal error: pkg is not analyzed!");
        }
        currentScope->importedPackages.push_back(pkgScope);
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
                assert(0);
            }
            auto importedPkgScope = pkgToScopeMap[importedPkg];
            pkgToScopeMap[currentPackage]->importedPackages.push_back(
                importedPkgScope);
        }
        else if (importedLibrary)
        {
            for (auto impPkg : importedLibrary->packages)
            {
                auto importedPkgScope = pkgToScopeMap[impPkg];
                pkgToScopeMap[currentPackage]->importedPackages.push_back(
                    importedPkgScope);
            }
            currentScope->importedLibs.push_back(importedLibrary);
        }
    }
}

void SemanticAnalyzer::AnalyzeDeferStatement(DeferStatement *deferStmt)
{
    deferredStatementsMap[currentBlockLeaf].push_back(
        deferStmt->GetStatement());
}
void SemanticAnalyzer::AnalyzeFnExpression(FnExpression *fnExpr)
{
    Function *fp = gZtoonArena.Allocate<Function>();
    fp->fnExpr = fnExpr;
    fp->name = std::format("__anonymous_fn_{}", (size_t)fnExpr);
    fnExpr->name = fp->name;
    FnDataType *fpDataType = gZtoonArena.Allocate<FnDataType>();
    fpDataType->fn = fp;
    fpDataType->type = DataType::Type::FN;
    fpDataType->returnDataType =
        currentScope->GetDataType(fnExpr->returnDataTypeToken);
    fpDataType->isVarArgs = fnExpr->IsVarArgs();
    exprToDataTypeMap[fnExpr] = fpDataType;
    fp->fnPointer = fpDataType->GetFnPtrType();

    currentScope->AddSymbol(fp, fnExpr->GetCodeErrString());

    auto tempScope = currentScope;

    currentScope = gZtoonArena.Allocate<Scope>(
        this, std::format("{}_{}", fnExpr->GetName(), (size_t)fnExpr),
        (SourceLocation){fnExpr->GetFirstToken()->GetFilepath(),
                         (uint32_t)fnExpr->GetFirstToken()->GetLineNumber(),
                         (uint32_t)fnExpr->GetFirstToken()->GetColNumber()});
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
            std::string leftStr = binaryExpr->left->GetCodeErrString().str;
            std::string rightStr = binaryExpr->right->GetCodeErrString().str;
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

    if (valueType->GetType() == DataType::Type::STRUCT ||
        castType->GetType() == DataType::Type::STRUCT ||
        valueType->GetType() == DataType::Type::UNION ||
        castType->GetType() == DataType::Type::UNION)
    {
        ReportError(
            std::format("Invalid Casting from '{}' datatype to '{}' datatype",
                        valueType->ToString(), castType->ToString()),
            castExpr->GetCodeErrString());
    }

    if (castType->GetType() == DataType::Type::BOOL)
    {
        if (exprToDataTypeMap[castExpr]->GetType() != DataType::Type::BOOL)
        {
            ReportError(
                std::format(
                    "Invalid Casting from '{}' datatype to '{}' datatype",
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

            if (currentBlockLeaf)
            {
                // inside
                size_t s = currentBlockLeaf->blockStmt->statements.size();
                currentBlockLeaf->blockStmt->statements.insert(
                    currentBlockLeaf->blockStmt->statements.begin() +
                        (long long)currentBlockLeaf->blockStmt->index + 1,
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
    if (fnCallExpr->generic)
    {
        auto pe =
            dynamic_cast<PrimaryExpression *>(fnCallExpr->GetGetExpression());
        auto maExpr = dynamic_cast<MemberAccessExpression *>(
            fnCallExpr->GetGetExpression());
        if (!pe)
        {
            if (!maExpr)
            {
                ReportError("Missing identifier",
                            fnCallExpr->GetCodeErrString());
            }
            pe =
                dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());
        }

        if ((!pe || pe->GetPrimary()->GetType() != TokenType::IDENTIFIER))
        {
            ReportError("Missing identifier", fnCallExpr->GetCodeErrString());
        }
        Scope *methodScope = nullptr;
        StructDataType *structType = nullptr;
        if (maExpr)
        {
            structType = dynamic_cast<StructDataType *>(
                stmtToDataTypeMap[currentStruct]);
            methodScope = structType->scope;
        }

        Function *fn = dynamic_cast<Function *>(
            (structType ? methodScope : currentScope)
                ->GetSymbol(pe->primary->GetLexeme(), pe->GetCodeErrString()));

        if (!fn->generic)
        {
            ReportError(std::format("{} '{}' is not generic",
                                    fn->isMethod ? "Method" : "Function",
                                    fn->GetName()),
                        fnCallExpr->GetCodeErrString());
        }
        if (fnCallExpr->generic->types.size() !=
            fn->fnStmt->generic->types.size())
        {
            ReportError(std::format("Wrong number of types provided to generic "
                                    "{} '{}'",
                                    fn->isMethod ? "Method" : "Function",
                                    fn->GetName()),
                        fnCallExpr->GetCodeErrString());
        }
        std::string newName = fn->GetName();
        // for (auto dtToken : fnCallExpr->generic->types)
        // {
        //     newName += std::format("_{}", dtToken->ToString());
        // }

        std::vector<DataType *> gTypes;
        for (auto t : fnCallExpr->generic->types)
        {
            gTypes.push_back(currentScope->GetDataType(t));
        }

        std::function<std::string(std::string, std::vector<DataType *> &)>
            genName = nullptr;
        genName = [](std::string typeName,
                     std::vector<DataType *> &gTypes) -> std::string
        {
            std::string name = typeName;
            for (auto type : gTypes)
            {
                name += std::format("_{}", type->ToString());
            }
            return name;
        };

        newName = genName(newName, gTypes);

        Token *newID = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
        *newID = *pe->primary;
        newID->lexeme = newName;
        pe->primary = newID;

        if (Symbol *fnFound = currentScope->GetSymbol(
                newName, fnCallExpr->GetCodeErrString(), true))
        {
            Function *found = dynamic_cast<Function *>(fnFound);
            fnCallExpr->generic = nullptr;
        }
        else
        {
            std::unordered_map<std::string, DataTypeToken *> dataTypesMap;

            for (size_t i = 0; i < fn->fnStmt->generic->types.size(); i++)
            {
                dataTypesMap[fn->fnStmt->generic->types[i]->ToString()] =
                    fnCallExpr->generic->types[i];
            }
            auto tokens = fn->fnStmt->tokens;
            std::vector<Token *> processTokens = tokens.tokens;
            auto generic = fn->fnStmt->generic;
            ReplaceGenericTypes(dataTypesMap);

            ReplaceGenericTypesInStatement(tokens, generic, processTokens,
                                           dataTypesMap);

            for (auto itr = processTokens.begin() + (long long)tokens.startPos;
                 itr != processTokens.begin() + (long long)tokens.endPos; itr++)
            {
                bool doneRemoving = false;
                if ((*itr)->GetType() == TokenType::LEFT_ANGLE_SQUARE)
                {
                    size_t layer = 1;
                    itr = processTokens.erase(itr);
                    while (layer != 0)
                    {
                        if ((*itr)->GetType() == TokenType::LEFT_ANGLE_SQUARE)
                        {
                            layer++;
                        }
                        else if ((*itr)->GetType() ==
                                 TokenType::RIGHT_SQUARE_ANGLE)
                        {
                            layer--;
                        }

                        itr = processTokens.erase(itr);
                    }
                    doneRemoving = true;
                }
                if (doneRemoving && (*itr)->GetType() == TokenType::IDENTIFIER)
                {
                    auto oldID = *itr;
                    *itr = gZtoonArena.Allocate<Token>(TokenType::IDENTIFIER);
                    *(*itr) = *oldID;
                    (*itr)->lexeme = newName;
                    break;
                }
            }
            Parser parser(processTokens);
            parser.currentIndex = tokens.startPos;

            FnStatement *newStatement = dynamic_cast<FnStatement *>(
                parser.ParseFnStatement(structType));

            if (!newStatement)
            {
                ReportError("Failed to generate generic type",
                            fn->fnStmt->GetCodeErrString());
            }

            GenericStatementInfo temp = {};

            temp.currentScope = currentScope;
            temp.currentBlockLeaf = currentBlockLeaf;
            temp.currentFunction = currentFunction;
            temp.currentLibrary = currentLibrary;
            temp.currentPackage = currentPackage;

            GenericStatementInfo *genericInfo = fn->generic;
            Statement *genericStatement = fn->fnStmt;

            currentScope = structType ? methodScope : genericInfo->currentScope;
            currentBlockLeaf = genericInfo->currentBlockLeaf;
            currentFunction = genericInfo->currentFunction;
            currentLibrary = genericInfo->currentLibrary;
            currentPackage = genericInfo->currentPackage;

            AnalyzeFnStatement(newStatement, currentFunction, true, false,
                               fn->fnStmt->method);
            AnalyzeFnStatement(newStatement, currentFunction, false, true,
                               fn->fnStmt->method);
            // Get location
            if (!structType)
            {
                auto itr = std::find(currentPackage->GetStatements().begin(),
                                     currentPackage->GetStatements().end(),
                                     genericStatement);
                if (itr == currentPackage->GetStatements().end())
                {
                    PrintError("Internal error: generic statement "
                               "cannot be found");
                }
                long long index = itr - currentPackage->GetStatements().begin();
                stmtsToAdd.push_back(
                    {currentPackage->GetStatements(), index, newStatement});
            }

            currentScope = temp.currentScope;
            currentBlockLeaf = temp.currentBlockLeaf;
            currentFunction = temp.currentFunction;
            currentLibrary = temp.currentLibrary;
            currentPackage = temp.currentPackage;
            if (structType)
            {
                structType->structStmt->methods.push_back(newStatement);
            }
            fnCallExpr->generic = nullptr;
        }
    }
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

    bool isMethod = (fnDataType->fn && fnDataType->fn->fnStmt &&
                     fnDataType->fn->fnStmt->method);
    if (isMethod)
    {
        if (fnCallExpr->GetArgs().size() != fnDataType->GetParameters().size())
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
    }
    for (Expression *arg : fnCallExpr->args)
    {
        AnalyzeExpression(arg);
    }

    size_t fnSize = fnCallExpr->GetArgs().size();
    size_t fnTypeSize = fnDataType->GetParameters().size();
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
    StructDataType *structTypeFound = nullptr;
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
                assert(0);
                // auto tempPkg = currentPackage;
                // auto tempScope = currentScope;
                // AnalyzePackage(pkgFound);
                // currentPackage = tempPkg;
                // currentScope = tempScope;
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

        if (!pkgFound && !libFound)
        {
            structTypeFound =
                dynamic_cast<StructDataType *>(currentScope->GetSymbol(
                    primaryExpression->GetPrimary()->GetLexeme(),
                    primaryExpression->GetCodeErrString(), true));
        }
    }

    if (!pkgFound && !libFound && !structTypeFound)
    {
        AnalyzeExpression(maExpr->GetLeftExpression());
    }
    if (structTypeFound)
    {
        exprToDataTypeMap[primaryExpression] = structTypeFound;
    }
    DataType *left = exprToDataTypeMap[maExpr->GetLeftExpression()];

    if (maExpr->token->GetType() == TokenType::DOUBLE_COLON)
    {
        if (left->GetType() != DataType::Type::ENUM &&
            left->GetType() != DataType::Type::PACKAGE &&
            left->GetType() != DataType::Type::LIBRARY &&
            left->GetType() != DataType::Type::STRUCT)
        {
            ReportError("'::' operator is only used on 'enum', 'package', and "
                        "non-methods members of a struct",
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

        auto symbol =
            currentScope->GetSymbol(packageMember->GetPrimary()->GetLexeme(),
                                    packageMember->GetCodeErrString());
        if (!symbol->IsPublic())
        {
            ReportError(std::format("type '{}' is private",
                                    packageMember->GetPrimary()->GetLexeme()),
                        packageMember->GetCodeErrString());
        }

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
        currentStruct = structStmt;

        PrimaryExpression *field =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        if (!field || field->primary->GetType() != TokenType::IDENTIFIER)
        {
            ReportError(
                std::format("Right expression of '.' must be an identifier"),
                maExpr->GetCodeErrString());
        }
        // Check if struct has this field

        VarDeclStatement *varField = nullptr;

        for (auto v : structStmt->GetFields())
        {
            if (v->GetIdentifier()->GetLexeme() ==
                field->GetPrimary()->GetLexeme())
            {
                varField = v;
                break;
            }
        }

        UnionStatement *anonymousUnionStmt = nullptr;

        bool uFound = false;
        for (auto u : structStmt->unions)
        {
            auto uType = dynamic_cast<UnionDataType *>(stmtToDataTypeMap[u]);
            uType->scope->lookUpParent = false;
            auto symbol =
                uType->scope->GetSymbol(field->GetPrimary()->GetLexeme(),
                                        field->GetCodeErrString(), true);
            uType->scope->lookUpParent = true;
            if (symbol)
            {
                anonymousUnionStmt = u;
                varField = dynamic_cast<Variable *>(symbol)->varDeclStmt;
                break;
            }
        }

        FnStatement *methodField = nullptr;

        for (auto m : structStmt->methods)
        {
            if (m->GetIdentifier()->GetLexeme() ==
                field->GetPrimary()->GetLexeme())
            {
                methodField = m;
                break;
            }
        }

        if (!varField && !methodField)
        {
            ReportError(std::format("Struct type '{}' does not have "
                                    "field with name '{}'",
                                    structType->name,
                                    field->GetPrimary()->GetLexeme()),
                        field->GetCodeErrString());
        }

        if (anonymousUnionStmt)
        {
            MemberAccessExpression *expr =
                gZtoonArena.Allocate<MemberAccessExpression>();
            expr->token = maExpr->token;
            expr->accessType = MemberAccessExpression::AccessType::STRUCT;
            auto uType = dynamic_cast<UnionDataType *>(
                stmtToDataTypeMap[anonymousUnionStmt]);
            VarDeclStatement *variable = nullptr;
            for (auto f : structStmt->fields)
            {
                if (f->GetIdentifier()->GetLexeme() ==
                    uType->GetName() + "_field")
                {
                    variable = f;
                    break;
                }
            }
            auto rightPE = gZtoonArena.Allocate<PrimaryExpression>();
            rightPE->primary = variable->GetIdentifier();
            expr->leftExpr = maExpr->leftExpr;
            expr->rightExpr = rightPE;
            maExpr->leftExpr = expr;
            maExpr->accessType = MemberAccessExpression::AccessType::UNION;
            AnalyzeMemberAccessExpression(maExpr);
        }
        else
        {
            Scope *temp = currentScope;
            currentScope = structType->scope;
            currentScope->lookUpParent = false;
            AnalyzeExpression(maExpr->GetRightExpression());
            DataType *rightDataType = exprToDataTypeMap[field];
            if (methodField)
            {
                PointerDataType *fnPtrType = dynamic_cast<PointerDataType *>(
                    exprToDataTypeMap[maExpr->GetRightExpression()]);
                FnDataType *fnDataType =
                    dynamic_cast<FnDataType *>(fnPtrType->dataType);
                if (fnDataType->IsMethod())
                {
                    methodToCallerMap[maExpr] = maExpr->GetLeftExpression();
                }
                else
                {
                    if (maExpr->token->GetType() != TokenType::DOUBLE_COLON)
                    {
                        ReportError(
                            std::format("Function '{}' is not a method. To "
                                        "invoke it use '{}::{}' instead",
                                        field->GetPrimary()->GetLexeme(),
                                        structType->GetName(),
                                        field->GetPrimary()->GetLexeme()),
                            field->GetCodeErrString());
                    }
                }
            }
            exprToDataTypeMap[maExpr] = rightDataType;
            currentScope = temp;
            currentScope->lookUpParent = true;
        }
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

        VarDeclStatement *varField = nullptr;

        for (auto v : unionStmt->GetFields())
        {
            auto var = dynamic_cast<VarDeclStatement *>(v);
            if (var && var->GetIdentifier()->GetLexeme() ==
                           field->GetPrimary()->GetLexeme())
            {
                varField = var;
                break;
            }
        }

        StructStatement *anonymousStruct = nullptr;

        for (auto s : unionStmt->structs)
        {
            auto sType = dynamic_cast<StructDataType *>(stmtToDataTypeMap[s]);
            sType->scope->lookUpParent = false;
            auto symbol =
                sType->scope->GetSymbol(field->GetPrimary()->GetLexeme(),
                                        field->GetCodeErrString(), true);
            sType->scope->lookUpParent = true;
            if (symbol)
            {
                anonymousStruct = dynamic_cast<StructStatement *>(s);
                varField = dynamic_cast<Variable *>(symbol)->varDeclStmt;
                break;
            }
        }

        if (!varField)
        {
            ReportError(std::format("Union type '{}' does not have "
                                    "field with name '{}'",
                                    unionType->name,
                                    field->GetPrimary()->GetLexeme()),
                        field->GetCodeErrString());
        }

        if (anonymousStruct)
        {
            MemberAccessExpression *expr =
                gZtoonArena.Allocate<MemberAccessExpression>();
            expr->token = maExpr->token;
            expr->accessType = MemberAccessExpression::AccessType::UNION;
            auto sType = dynamic_cast<StructDataType *>(
                stmtToDataTypeMap[anonymousStruct]);
            VarDeclStatement *variable = nullptr;
            for (auto f : unionStmt->fields)
            {
                auto varDeclField = dynamic_cast<VarDeclStatement *>(f);
                if (varDeclField &&
                    varDeclField->GetIdentifier()->GetLexeme() ==
                        sType->GetName() + "_field")
                {
                    variable = varDeclField;
                    break;
                }
            }
            auto rightPE = gZtoonArena.Allocate<PrimaryExpression>();
            rightPE->primary = variable->GetIdentifier();
            expr->leftExpr = maExpr->leftExpr;
            expr->rightExpr = rightPE;
            maExpr->leftExpr = expr;
            maExpr->accessType = MemberAccessExpression::AccessType::STRUCT;
            AnalyzeMemberAccessExpression(maExpr);
        }
        else
        {

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
        else if (dynamic_cast<PointerDataType *>(leftDataType)->isNullPtr)
        {
            exprToDataTypeMap[*left] = rightDataType;
            leftDataType = rightDataType;
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
                // exprToDataTypeMap[castExpr]->isReadOnly =
                //     rightDataType->IsReadOnly();
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
