#include "error_report.h"
#include "lexer/lexer.h"
#include "parser.h"
#include <algorithm>
#include <format>
std::string DataTypeToken::ToString()
{
    std::string str;
    if (readOnly)
    {
        str += "readonly ";
    }

    if (arrayDesc)
    {
        auto arr = arrayDesc;
        while (arr)
        {
            str += arr->dataTypeToken->ToString();
            str += "[]";
            arr = arr->dataTypeToken->arrayDesc;
        }
    }
    else if (pointerDesc)
    {
        auto ptr = pointerDesc;
        while (ptr)
        {
            str += ptr->dataTypeToken->ToString();
            str += "*";
            ptr = ptr->dataTypeToken->pointerDesc;
        }
    }
    else
    {

        if (fnStatement)
        {
            str += "(fn";
            str += "(";
            for (auto t : fnStatement->GetParameters())
            {
                str += t->GetDataType()->ToString();
                str += ",";
            }
            if (fnStatement->IsVarArgs())
            {
                str += "...";
            }
            if (str.ends_with(','))
            {
                str.pop_back();
            }
            str += ")";

            if (fnStatement->GetReturnDatatype())
            {
                str += "->";
                str += fnStatement->GetReturnDatatype()->ToString();
            }
            str += ")";
        }
        else
        {
            str += dataType->GetLexeme();
        }
    }

    return str;
}

bool Parser::Consume(TokenType type)
{
    if (type == Peek()->GetType())
    {
        Advance();
        return true;
    }
    else
    {
        return false;
    }
}
Token const *Parser::Peek()
{
    if (currentIndex < tokens.size())
    {
        return tokens.at(currentIndex);
    }
    return nullptr;
}
Token const *Parser::PeekAhead(size_t steps)
{
    if ((currentIndex + steps) < tokens.size())
    {
        return tokens.at(currentIndex + steps);
    }
    return nullptr;
}
void Parser::Advance()
{
    if ((currentIndex + 1) < tokens.size())
        currentIndex++;
}

Token const *Parser::Prev()
{
    if ((currentIndex - 1) < tokens.size())
        return tokens.at(currentIndex - 1);
    else
        return nullptr;
}

DataTypeToken *Parser::ParseDataType(bool check)
{
    DataTypeToken *dataType = gZtoonArena.Allocate<DataTypeToken>();
    dataType->tokens.tokens = tokens;
    dataType->tokens.startPos = currentIndex;
    if (Consume(TokenType::READONLY))
    {
        dataType->readOnly = Prev();
    }
    if (Consume(TokenType::LEFT_PAREN))
    {

        if (Peek()->GetType() != (TokenType::FN))
        {
            CodeErrString ces = {};
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("Expected 'fn' after '('"), ces);
        }
        if (Consume(TokenType::FN))
        {
            FnStatement *fnStmt = gZtoonArena.Allocate<FnStatement>();
            fnStmt->fnToken = Prev();

            if (Consume(TokenType::LEFT_PAREN))
            {
                while (!Consume(TokenType::RIGHT_PAREN))
                {
                    Statement *varDeclStatement = ParseVarDeclStatement();
                    if (!dynamic_cast<VarDeclStatement *>(varDeclStatement))
                    {
                        if (Consume(TokenType::VAR_ARGS))
                        {
                            fnStmt->isVarArgs = true;
                            if (Consume(TokenType::RIGHT_PAREN))
                            {
                                break;
                            }
                            else
                            {
                                CodeErrString ces;
                                ces.firstToken = Prev();
                                ces.str = ces.firstToken->GetLexeme();
                                ReportError(
                                    std::format(
                                        "'...' token can only be the last "
                                        "function paramter"),
                                    ces);
                            }
                        }
                        else
                        {
                            ReportError(
                                std::format(
                                    "Expect function paramter but found '{}'",
                                    varDeclStatement->GetCodeErrString().str),
                                varDeclStatement->GetCodeErrString());
                        }
                    }
                    if (((VarDeclStatement *)varDeclStatement)->expression)
                    {
                        ReportError("Default parameters are not supported",
                                    varDeclStatement->GetCodeErrString());
                    }
                    ((VarDeclStatement *)varDeclStatement)->isParamter = true;
                    fnStmt->parameters.push_back(
                        (VarDeclStatement *)varDeclStatement);
                    Consume(TokenType::COMMA);
                }
            }

            if (Consume(TokenType::ARROW))
            {
                DataTypeToken *retDataType = ParseDataType();
                if (!retDataType)
                {
                    CodeErrString ces = {};
                    ces.firstToken = Prev();
                    ces.str = ces.firstToken->GetLexeme();

                    ReportError(
                        std::format("Expect datatype after '->'", ces.str),
                        ces);
                }

                fnStmt->returnDataTypeToken = retDataType;
            }
            else
            {
                DataTypeToken *noType = gZtoonArena.Allocate<DataTypeToken>();
                noType->dataType =
                    gZtoonArena.Allocate<Token>(TokenType::NOTYPE);
                fnStmt->returnDataTypeToken = noType;
            }
            dataType->fnStatement = fnStmt;

            if (!Consume(TokenType::RIGHT_PAREN))
            {
                CodeErrString ces = {};
                ces.firstToken = Peek();
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected ')' after 'fn'"), ces);
            }
        }
    }
    else if (IsDataType(Peek()->GetType()))
    {
        Advance();
        bool libAccess = false;
        const Token *firstAccessStage = Prev();
        const Token *secondAccessStage = nullptr;
        const Token *thirdAccessStage = nullptr;
        if (Consume(TokenType::DOUBLE_COLON))
        {
            if (IsDataType(Peek()->GetType()))
            {
                Advance();
                secondAccessStage = Prev();
                if (Consume(TokenType::DOUBLE_COLON))
                {
                    libAccess = true;
                    if (IsDataType(Peek()->GetType()))
                    {
                        Advance();
                        thirdAccessStage = Prev();
                    }
                    else
                    {
                        CodeErrString ces;
                        ces.firstToken = Peek();
                        ces.str = ces.firstToken->GetLexeme();
                        ReportError(
                            std::format("Expected 'identifier' after '::'"),
                            ces);
                    }
                }
            }
            else
            {
                CodeErrString ces;
                ces.firstToken = Peek();
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected 'identifier' after '::'"),
                            ces);
            }
        }
        else
        {
            firstAccessStage = Prev();
        }

        if (libAccess)
        {
            // lib::pkg::datatype
            dataType->libToken = firstAccessStage;
            dataType->pkgToken = secondAccessStage;
            if (IsDataType(thirdAccessStage->GetType()))
            {
                dataType->dataType = thirdAccessStage;
            }
            else
            {
                CodeErrString ces;
                ces.firstToken = thirdAccessStage;
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected 'datatype' after '::'"), ces);
            }
        }
        else if (secondAccessStage)
        {
            // pkg::datatype
            dataType->pkgToken = firstAccessStage;

            if (IsDataType(secondAccessStage->GetType()))
            {

                dataType->dataType = secondAccessStage;
            }
            else
            {
                CodeErrString ces;
                ces.firstToken = secondAccessStage;
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected 'datatype' after '::'"), ces);
            }
        }
        else
        {
            // datatype
            if (IsDataType(firstAccessStage->GetType()))
            {
                dataType->dataType = firstAccessStage;
            }
            else
            {
                CodeErrString ces;
                ces.firstToken = firstAccessStage;
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected 'datatype' after '::'"), ces);
            }
        }
        dataType->generic = ParseGeneric();
    }
    else if (!check)
    {
        CodeErrString ces = {};
        ces.firstToken = Peek();
        ces.str = ces.firstToken->GetLexeme();
        ReportError(std::format("Expect datatype"), ces);
    }
    while (Consume(TokenType::ASTERISK))
    {
        auto ptrDesc = gZtoonArena.Allocate<DataTypeToken::PointerDesc>();
        auto ptrType = gZtoonArena.Allocate<DataTypeToken>();
        *ptrType = *dataType;
        ptrType->fnStatement = nullptr;
        ptrType->arrayDesc = nullptr;
        dataType->readOnly = nullptr;
        ptrDesc->dataTypeToken = dataType;
        ptrDesc->token = Prev();
        ptrType->pointerDesc = ptrDesc;
        dataType = ptrType;
    }

    while (Consume(TokenType::LEFT_SQUARE_BRACKET))
    {
        DataTypeToken::ArrayDesc *arrDesc =
            gZtoonArena.Allocate<DataTypeToken::ArrayDesc>();
        arrDesc->token = Prev();
        arrDesc->arraySizeExpr = ParseExpression();
        auto innerDataType = gZtoonArena.Allocate<DataTypeToken>();
        *innerDataType = *dataType;
        innerDataType->fnStatement = nullptr;
        innerDataType->pointerDesc = nullptr;
        dataType->readOnly = nullptr;
        innerDataType->arrayDesc = arrDesc;
        arrDesc->dataTypeToken = dataType;
        dataType = innerDataType;
        if (!Consume(TokenType::RIGHT_SQUARE_BRACKET))
        {
            CodeErrString ces = {};
            ces.firstToken = arrDesc->token;
            ces.str = ces.firstToken->GetLexeme();

            ReportError("Missing ']'", ces);
        }
    }
    while (Consume(TokenType::ASTERISK))
    {
        auto ptrDesc = gZtoonArena.Allocate<DataTypeToken::PointerDesc>();
        auto ptrType = gZtoonArena.Allocate<DataTypeToken>();
        ptrDesc->dataTypeToken = dataType;
        ptrDesc->token = Prev();
        ptrType->dataType = dataType->dataType;
        ptrType->pointerDesc = ptrDesc;
        ptrType->readOnly = dataType->readOnly;
        ptrType->tokens = dataType->tokens;
        dataType->readOnly = nullptr;
        dataType = ptrType;
    }
    dataType->tokens.endPos = currentIndex;
    return dataType;
}

bool Parser::IsDataType(TokenType type)
{
    return ::IsDataType(type) || type == TokenType::IDENTIFIER;
}
Parser::Parser(std::vector<Token *> &tokens) : tokens(tokens)
{
    currentStage = Stage::PARSER;
}

Parser::~Parser() {}

std::vector<Package *> &Parser::Parse()
{
    while (!Consume(TokenType::END_OF_PROGRAM))
    {
        while (Consume(TokenType::END_OF_FILE))
        {
        }
        if (!Consume(TokenType::PACKAGE))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Source file must begin with 'package'", ces);
        }
        else
        {
            Package *package = gZtoonArena.Allocate<Package>();
            if (!Consume(TokenType::IDENTIFIER))
            {
                CodeErrString ces;
                ces.firstToken = Peek();
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Missing identifiers after 'package'", ces);
            }
            package->identifier = Prev();
            Consume(TokenType::SEMICOLON);
            while (Peek()->GetType() != TokenType::END_OF_FILE)
            {
                package->statements.push_back(ParseDeclaration());
            }
            packages.push_back(package);
            Consume(TokenType::END_OF_FILE);
        }
    }
    return packages;
}

Generic *Parser::ParseGeneric()
{
    Generic *generic = nullptr;
    if (Consume(TokenType::LEFT_ANGLE_SQUARE))
    {
        generic = gZtoonArena.Allocate<Generic>();

        while (!Consume(TokenType::RIGHT_SQUARE_ANGLE))
        {
            generic->types.push_back(ParseDataType());
            Consume(TokenType::COMMA);
        }
    }
    return generic;
}
Statement *Parser::ParseDeclaration()
{
    Statement *declStmt = ParseStatement();

    if (dynamic_cast<VarDeclStatement *>(declStmt) ||
        dynamic_cast<FnStatement *>(declStmt) ||
        dynamic_cast<StructStatement *>(declStmt) ||
        dynamic_cast<UnionStatement *>(declStmt) ||
        dynamic_cast<EnumStatement *>(declStmt) ||
        dynamic_cast<ImportStatement *>(declStmt))
    {
        VarDeclStatement *varDecl = dynamic_cast<VarDeclStatement *>(declStmt);
        if (varDecl)
        {
            varDecl->isGlobal = true;
        }
        return declStmt;
    }
    else
    {
        ReportError(
            std::format("Only function definition, variable declaration, "
                        "struct, enum, union definition and import statements "
                        "are allowed in "
                        "global scope"),
            declStmt->GetCodeErrString());
    }
    return nullptr;
}

Statement *Parser::ParseStatement()
{
    Statement *statement = ParseBlockStatement();

    if (!statement)
    {
        CodeErrString ces = {};
        ces.firstToken = Peek();
        ces.str = ces.firstToken->GetLexeme();
        ReportError(std::format("Unrecognized statement near '{}'",
                                Peek()->GetLexeme()),
                    ces);
    }

    if (dynamic_cast<BlockStatement *>(statement) ||
        dynamic_cast<IfStatement *>(statement) ||
        dynamic_cast<WhileLoopStatement *>(statement) ||
        dynamic_cast<ForLoopStatement *>(statement) ||
        dynamic_cast<FnStatement *>(statement) ||
        Consume(TokenType::SEMICOLON) ||
        dynamic_cast<StructStatement *>(statement) ||
        dynamic_cast<UnionStatement *>(statement) ||
        dynamic_cast<EnumStatement *>(statement) ||
        dynamic_cast<SwitchStatement *>(statement) ||
        dynamic_cast<DeferStatement *>(statement))
    {
        return statement;
    }
    ReportError("Expected ';' after statement.", statement->GetCodeErrString());
    return nullptr;
}

Statement *Parser::ParseBlockStatement()
{
    if (Consume(TokenType::LEFT_CURLY_BRACKET))
    {
        BlockStatement *blockStatement = gZtoonArena.Allocate<BlockStatement>();
        blockStatement->firstToken = Prev();
        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {
            blockStatement->statements.push_back(ParseStatement());
        }
        return blockStatement;
    }
    return ParseImportStatement();
}

Statement *Parser::ParseImportStatement()
{
    if (Consume(TokenType::IMPORT))
    {
        ImportStatement *importStmt = gZtoonArena.Allocate<ImportStatement>();
        importStmt->token = Prev();
        importStmt->package = ParseExpression();
        auto pe = dynamic_cast<PrimaryExpression *>(importStmt->package);
        auto ma = dynamic_cast<MemberAccessExpression *>(importStmt->package);

        if (!importStmt->package)
        {
            CodeErrString ces;
            ces.firstToken = importStmt->token;
            ces.str = ces.firstToken->GetLexeme();

            ReportError("Expected expression after 'import'", ces);
        }

        if (!(pe && pe->primary->GetType() == TokenType::IDENTIFIER) && !ma)
        {
            ReportError("Invalid expression after 'import'",
                        importStmt->GetCodeErrString());
        }
        if (ma)
        {
            ma->accessType = MemberAccessExpression::AccessType::PACKAGE;
        }

        return importStmt;
    }

    return ParseFnStatement();
}
Statement *Parser::ParseFnStatement(bool isMethod)
{

    Token const *pubToken = nullptr;
    size_t startPos = 0;
    if (Peek()->GetType() == TokenType::PUB &&
        PeekAhead(1)->GetType() == TokenType::FN)
    {
        startPos = currentIndex;
        Advance();
        pubToken = Prev();
    }
    if (Consume(TokenType::FN))
    {
        FnStatement *fnStmt = gZtoonArena.Allocate<FnStatement>();
        fnStmt->tokens.tokens = tokens;
        if (pubToken)
        {
            fnStmt->tokens.startPos = startPos;
        }
        else
        {
            fnStmt->tokens.startPos = currentIndex - 1;
        }
        fnStmt->pub = pubToken;
        fnStmt->fnToken = Prev();
        fnStmt->generic = ParseGeneric();
        PrimaryExpression *primaryExpr =
            dynamic_cast<PrimaryExpression *>(ParsePrimaryExpression());

        if (primaryExpr)
        {
            if (primaryExpr->primary->GetType() != TokenType::IDENTIFIER)
            {
                CodeErrString ces = {};
                ces.firstToken = fnStmt->fnToken;
                ces.str = ces.firstToken->GetLexeme();
                ReportError(
                    std::format("Expect identitifer after 'fn' but found '{}'",
                                primaryExpr->GetCodeErrString().str),
                    ces);
            }
        }
        else
        {
            CodeErrString ces = {};
            ces.firstToken = fnStmt->fnToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("Expect identitifer after 'fn'"), ces);
        }
        fnStmt->identifier = primaryExpr->GetPrimary();

        if (Consume(TokenType::LEFT_PAREN))
        {
            if (isMethod)
            {
                if (Consume(TokenType::READONLY))
                {
                    fnStmt->method =
                        gZtoonArena.Allocate<FnStatement::Method>();
                    fnStmt->method->readonly = Prev();
                }
                if (Peek()->GetType() == TokenType::IDENTIFIER &&
                    Peek()->GetLexeme() != "self")
                {
                    if (fnStmt->method)
                    {
                        CodeErrString ces;
                        ces.firstToken = Prev();
                        ces.str = ces.firstToken->GetLexeme();
                        ReportError(std::format("Expected 'self' after '{}'",
                                                Prev()->GetLexeme()),
                                    ces);
                    }
                }
                else
                {
                    Consume(TokenType::IDENTIFIER);
                    if (!fnStmt->method)
                    {
                        fnStmt->method =
                            gZtoonArena.Allocate<FnStatement::Method>();
                    }
                    fnStmt->method->selfToken = Prev();

                    if (Consume(TokenType::ASTERISK))
                    {
                        fnStmt->method->asterisk = Prev();
                    }
                    Consume(TokenType::COMMA);
                }
            }
            while (!Consume(TokenType::RIGHT_PAREN))
            {
                Statement *varDeclStatement = ParseVarDeclStatement();
                if (!dynamic_cast<VarDeclStatement *>(varDeclStatement))
                {
                    if (Consume(TokenType::VAR_ARGS))
                    {
                        fnStmt->isVarArgs = true;
                        if (Consume(TokenType::RIGHT_PAREN))
                        {
                            break;
                        }
                        else
                        {
                            CodeErrString ces;
                            ces.firstToken = Prev();
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError(
                                std::format("'...' token can only be the last "
                                            "function paramter"),
                                ces);
                        }
                    }
                    else
                    {
                        ReportError(
                            std::format(
                                "Expect function paramter but found '{}'",
                                varDeclStatement->GetCodeErrString().str),
                            varDeclStatement->GetCodeErrString());
                    }
                }
                if (((VarDeclStatement *)varDeclStatement)->expression)
                {
                    ReportError("Default parameters are not supported",
                                varDeclStatement->GetCodeErrString());
                }
                ((VarDeclStatement *)varDeclStatement)->isParamter = true;
                fnStmt->parameters.push_back(
                    (VarDeclStatement *)varDeclStatement);
                Consume(TokenType::COMMA);
            }
        }
        else
        {
            ReportError(std::format("Expect '(' after '{}'",
                                    primaryExpr->GetCodeErrString().str),
                        primaryExpr->GetCodeErrString());
        }

        if (Consume(TokenType::ARROW))
        {
            DataTypeToken *retDataType = ParseDataType();
            if (!retDataType)
            {
                CodeErrString ces = {};
                ces.firstToken = Prev();
                ces.str = ces.firstToken->GetLexeme();

                ReportError(std::format("Expect datatype after '->'", ces.str),
                            ces);
            }

            fnStmt->returnDataTypeToken = retDataType;
        }
        else
        {
            DataTypeToken *noType = gZtoonArena.Allocate<DataTypeToken>();
            noType->dataType = gZtoonArena.Allocate<Token>(TokenType::NOTYPE);
            fnStmt->returnDataTypeToken = noType;
        }

        if (Consume(TokenType::SEMICOLON))
        {
            fnStmt->isPrototype = true;
            fnStmt->blockStatement = gZtoonArena.Allocate<BlockStatement>();
            fnStmt->blockStatement->firstToken = Peek();
            for (Statement *s : fnStmt->parameters)
            {
                fnStmt->blockStatement->statements.push_back(s);
            }

            return fnStmt;
        }

        Statement *blockStatement = ParseBlockStatement();

        if (!dynamic_cast<BlockStatement *>(blockStatement))
        {
            ReportError(std::format("Expect block statement but found '{}'",
                                    blockStatement->GetCodeErrString().str),
                        blockStatement->GetCodeErrString());
        }

        fnStmt->blockStatement = (BlockStatement *)blockStatement;
        fnStmt->tokens.endPos = currentIndex;
        return fnStmt;
    }
    return ParseStructStatement();
}
Statement *Parser::ParseStructStatement(bool anonymous)
{

    Token const *pubToken = nullptr;
    size_t startPos = 0;
    if (Peek()->GetType() == TokenType::PUB &&
        PeekAhead(1)->GetType() == TokenType::STRUCT)
    {
        startPos = currentIndex;
        Advance();
        pubToken = Prev();
    }
    if (Consume(TokenType::STRUCT))
    {
        StructStatement *structStmt = gZtoonArena.Allocate<StructStatement>();
        structStmt->tokens.tokens = tokens;
        if (pubToken)
        {
            structStmt->tokens.startPos = startPos;
        }
        else
        {
            structStmt->tokens.startPos = currentIndex - 1;
        }
        structStmt->pub = pubToken;
        structStmt->token = Prev();
        if (!anonymous && !Consume(TokenType::IDENTIFIER))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected identifier after 'struct'", ces);
        }
        if (!anonymous)
        {
            structStmt->identifier = Prev();
            structStmt->generic = ParseGeneric();
        }
        if (!Consume(TokenType::LEFT_CURLY_BRACKET))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected '{' after 'struct' identifier", ces);
        }

        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {
            isAnonymous = true;
            Statement *fieldStmt = ParseFnStatement(true);
            isAnonymous = false;
            FnStatement *fnStmt = dynamic_cast<FnStatement *>(fieldStmt);
            VarDeclStatement *varDeclStmt =
                dynamic_cast<VarDeclStatement *>(fieldStmt);
            UnionStatement *unionStmt =
                dynamic_cast<UnionStatement *>(fieldStmt);
            if (!varDeclStmt && !fnStmt && !unionStmt)
            {
                auto fieldStmt = ParseStatement();
                ReportError("Statement not supported in struct definition",
                            fieldStmt->GetCodeErrString());
            }
            if (varDeclStmt)
            {
                if (anonymous && varDeclStmt->GetExpression())
                {
                    ReportError("Anonymous structs cannot have default values",
                                varDeclStmt->GetCodeErrString());
                }
                varDeclStmt->isField = true;
                structStmt->fieldsInOrder.push_back(varDeclStmt);
                structStmt->fields.push_back(varDeclStmt);
            }
            else if (unionStmt)
            {
                if (!unionStmt->identifier)
                {
                    ReportError("Only anonymous unions are allowd to be "
                                "decalred in struct definition",
                                unionStmt->GetCodeErrString());
                }
                structStmt->fieldsInOrder.push_back(unionStmt);
                structStmt->unions.push_back(unionStmt);
            }
            else if (fnStmt)
            {
                if (fnStmt->method)
                {
                    AddSelfParam(fnStmt, structStmt->identifier);
                }

                fnStmt->structStmt = structStmt;
                structStmt->methods.push_back(fnStmt);
            }
            if (varDeclStmt && !Consume(TokenType::SEMICOLON))
            {
                ReportError("Expedted ';' after field declaration",
                            varDeclStmt->GetCodeErrString());
            }
        }

        if (anonymous && !structStmt->GetMethods().empty())
        {
            ReportError(std::format("Methods cannot be declared in an "
                                    "anonymous struct"),
                        structStmt->GetCodeErrString());
        }

        structStmt->tokens.endPos = currentIndex;
        return structStmt;
    }
    return ParseUnionStatement();
}

void Parser::AddSelfParam(FnStatement *method, const Token *dataType)
{
    // add param
    auto selfParam = gZtoonArena.Allocate<VarDeclStatement>();
    selfParam->identifier = method->method->selfToken;
    selfParam->isParamter = true;
    DataTypeToken *dataTypeToken = gZtoonArena.Allocate<DataTypeToken>();
    dataTypeToken->dataType = dataType;
    if (method->method->asterisk)
    {
        DataTypeToken *ptrDataTypeToken = gZtoonArena.Allocate<DataTypeToken>();
        ptrDataTypeToken->pointerDesc =
            gZtoonArena.Allocate<DataTypeToken::PointerDesc>();
        ptrDataTypeToken->pointerDesc->dataTypeToken = dataTypeToken;
        ptrDataTypeToken->pointerDesc->token = dataType;
        dataTypeToken = ptrDataTypeToken;
    }
    else
    {
        dataTypeToken->dataType = dataType;
    }
    dataTypeToken->readOnly = method->method->readonly;
    selfParam->dataTypeToken = dataTypeToken;
    method->GetParameters().insert(method->GetParameters().begin(), selfParam);
}

Statement *Parser::ParseUnionStatement()
{

    Token const *pubToken = nullptr;
    size_t startPos = 0;
    if (Peek()->GetType() == TokenType::PUB &&
        PeekAhead(1)->GetType() == TokenType::UNION)
    {
        startPos = currentIndex;
        Advance();
        pubToken = Prev();
    }
    if (Consume(TokenType::UNION))
    {
        UnionStatement *unionStmt = gZtoonArena.Allocate<UnionStatement>();
        unionStmt->tokens.tokens = tokens;
        if (pubToken)
        {
            unionStmt->tokens.startPos = startPos;
        }
        else
        {
            unionStmt->tokens.startPos = currentIndex - 1;
        }
        unionStmt->pub = pubToken;
        unionStmt->token = Prev();
        if (!Consume(TokenType::IDENTIFIER) && !isAnonymous)
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected identifier after 'Union'", ces);
        }
        if (isAnonymous)
        {
            isAnonymous = false;
        }
        if (!isAnonymous)
        {
            unionStmt->identifier = Prev();
            unionStmt->generic = ParseGeneric();
        }
        else
        {
            unionStmt->identifier = Prev();
        }

        if (!Consume(TokenType::LEFT_CURLY_BRACKET))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected '{' after 'union' identifier", ces);
        }

        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {

            VarDeclStatement *varDeclStmt =
                dynamic_cast<VarDeclStatement *>(ParseVarDeclStatement());
            StructStatement *structStmt;
            if (!varDeclStmt)
            {
                structStmt =
                    dynamic_cast<StructStatement *>(ParseStructStatement(true));
            }
            if (varDeclStmt)
            {
                varDeclStmt->isField = true;
                unionStmt->fieldsInOrder.push_back(varDeclStmt);
                unionStmt->fields.push_back(varDeclStmt);
            }
            else if (structStmt)
            {
                unionStmt->fieldsInOrder.push_back(structStmt);
                unionStmt->structs.push_back(structStmt);
            }
            else
            {
                CodeErrString ces = {};
                ces.firstToken = Peek();
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Statement not supported in union definition", ces);
            }

            if (varDeclStmt && varDeclStmt->GetExpression())
            {
                ReportError("Union fileds cannot have default values",
                            varDeclStmt->GetCodeErrString());
            }

            if (!Consume(TokenType::SEMICOLON) && varDeclStmt)
            {
                ReportError("Expected ';' after field declaration",
                            varDeclStmt->GetCodeErrString());
            }
        }
        unionStmt->tokens.endPos = currentIndex;
        return unionStmt;
    }
    return ParseEnumStatement();
}

Statement *Parser::ParseEnumStatement()
{

    Token const *pubToken = nullptr;
    if (Peek()->GetType() == TokenType::PUB &&
        PeekAhead(1)->GetType() == TokenType::ENUM)
    {
        Advance();
        pubToken = Prev();
    }
    if (Consume(TokenType::ENUM))
    {
        EnumStatement *enumStmt = gZtoonArena.Allocate<EnumStatement>();
        enumStmt->pub = pubToken;
        enumStmt->token = Prev();

        if (!Consume(TokenType::IDENTIFIER))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected identifier after 'enum'", ces);
        }

        enumStmt->identifier = Prev();

        if (!Consume(TokenType::COLON))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected ':' after 'enum' identifier", ces);
        }

        enumStmt->datatype = ParseDataType();

        if (!enumStmt->datatype)
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected 'datatype' after ':'", ces);
        }
        enumStmt->datatype->readOnly =
            gZtoonArena.Allocate<Token>(TokenType::READONLY);

        if (!Consume(TokenType::LEFT_CURLY_BRACKET))
        {
            CodeErrString ces;
            ces.firstToken = Peek();
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected '{' after 'enum' identifier", ces);
        }

        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {

            EnumStatement::Field *field =
                gZtoonArena.Allocate<EnumStatement::Field>();
            if (!Consume(TokenType::IDENTIFIER))
            {
                CodeErrString ces;
                ces.firstToken = Peek();
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Expected identifier after 'enum'", ces);
            }
            field->identifier = Prev();
            if (Consume(TokenType::EQUAL))
            {
                field->expr = ParseExpression();
                if (!field->expr)
                {
                    CodeErrString ces;
                    ces.firstToken = Peek();
                    ces.str = ces.firstToken->GetLexeme();
                    ReportError("Expected expression after '='", ces);
                }
            }

            auto itr =
                std::find_if(enumStmt->fields.begin(), enumStmt->fields.end(),
                             [field](EnumStatement::Field *other) {
                                 return field->identifier->GetLexeme() ==
                                        other->identifier->GetLexeme();
                             });
            if (itr != enumStmt->fields.end())
            {
                CodeErrString ces;
                ces.firstToken = field->identifier;
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Field already defined", ces);
            }
            enumStmt->fields.push_back(field);
            Consume(TokenType::COMMA);
        }

        return enumStmt;
    }
    return ParseVarDeclStatement();
}
Statement *Parser::ParseVarDeclStatement()
{
    Token const *pubToken = nullptr;
    if (Peek()->GetType() == TokenType::PUB &&
        PeekAhead(1)->GetType() == TokenType::IDENTIFIER &&
        PeekAhead(2)->GetType() == TokenType::COLON)
    {
        Advance();
        pubToken = Prev();
    }

    if (Peek()->GetType() == TokenType::IDENTIFIER &&
        PeekAhead(1)->GetType() == TokenType::COLON)
    {
        Advance();
        VarDeclStatement *varDeclStatement =
            gZtoonArena.Allocate<VarDeclStatement>();
        varDeclStatement->pub = pubToken;
        varDeclStatement->identifier = Prev();
        if (Consume(TokenType::COLON))
        {
            DataTypeToken *dataType = ParseDataType();
            if (dataType)
            {
                varDeclStatement->dataTypeToken = dataType;
                if (Consume(TokenType::EQUAL))
                {

                    varDeclStatement->expression = ParseExpression();
                    if (!varDeclStatement->expression)
                    {
                        ReportError("Expected expression after '='.",
                                    varDeclStatement->GetCodeErrString());
                        return nullptr;
                    }
                }
            }
            else
            {
                CodeErrString ces = {};
                ces.firstToken = varDeclStatement->GetIdentifier();
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Expected datatype after ':'", ces);
            }
        }
        return varDeclStatement;
    }
    return ParseSwitchStatement();
}
Statement *Parser::ParseSwitchStatement()
{
    while (Consume(TokenType::SWITCH))
    {
        SwitchStatement *switchStmt = gZtoonArena.Allocate<SwitchStatement>();
        switchStmt->token = Prev();

        switchStmt->matchExpr = ParseExpression();

        if (!switchStmt->matchExpr)
        {
            CodeErrString ces;
            ces.firstToken = switchStmt->token;
            ces.str = ces.firstToken->GetLexeme();
            ReportError("expected expression after 'switch'", ces);
        }

        if (!Consume(TokenType::LEFT_CURLY_BRACKET))
        {
            ReportError("expected '{' after expression",
                        switchStmt->matchExpr->GetCodeErrString());
        }

        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {
            if (Consume(TokenType::CASE))
            {
                const Token *caseToken = Prev();
                SwitchStatement::Case *switchCase =
                    gZtoonArena.Allocate<SwitchStatement::Case>();

                while (!Consume(TokenType::COLON))
                {
                    Expression *e = ParseExpression();
                    if (!e)
                    {
                        CodeErrString ces;
                        ces.firstToken = Peek();
                        ces.str = ces.firstToken->GetLexeme();
                        ReportError(
                            std::format("expected expression after '{}'",
                                        Peek()->GetLexeme()),
                            ces);
                    }
                    switchCase->exprs.push_back(e);
                    Consume(TokenType::COMMA);
                }

                switchCase->blockStatement =
                    dynamic_cast<BlockStatement *>(ParseBlockStatement());
                if (!switchCase->blockStatement)
                {
                    CodeErrString ces;
                    ces.firstToken = caseToken;
                    ces.str = ces.firstToken->GetLexeme();
                    ReportError("Case's block statement is missing", ces);
                }

                switchStmt->cases.push_back(switchCase);
            }
            else if (Consume(TokenType::DEFAULT))
            {
                const Token *defaultToken = Prev();
                SwitchStatement::Case *switchDefault =
                    gZtoonArena.Allocate<SwitchStatement::Case>();

                if (!Consume(TokenType::COLON))
                {
                    CodeErrString ces;
                    ces.firstToken = defaultToken;
                    ces.str = ces.firstToken->GetLexeme();
                    ReportError("Expected ':' after 'default'", ces);
                }

                switchDefault->blockStatement =
                    dynamic_cast<BlockStatement *>(ParseBlockStatement());
                if (!switchDefault->blockStatement)
                {
                    CodeErrString ces;
                    ces.firstToken = defaultToken;
                    ces.str = ces.firstToken->GetLexeme();
                    ReportError("Default's block statement is missing", ces);
                }
                switchStmt->defualtCase = switchDefault;
            }
        }
        return switchStmt;
    }
    return ParseIfStatement();
}
Statement *Parser::ParseIfStatement()
{
    if (Consume(TokenType::IF))
    {
        IfStatement *ifStatement = gZtoonArena.Allocate<IfStatement>();
        ifStatement->ifToken = Prev();
        ifStatement->expression = ParseExpression();

        if (!ifStatement->expression)
        {
            CodeErrString ces;
            ces.firstToken = ifStatement->ifToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected expression after 'if'", ces);
        }

        ifStatement->blockStatement =
            dynamic_cast<BlockStatement *>(ParseBlockStatement());

        if (!dynamic_cast<BlockStatement *>(ifStatement->blockStatement))
        {
            ReportError("If statement's block is missing",
                        ifStatement->expression->GetCodeErrString());
        }
        while (Peek()->GetType() == TokenType::ELSE &&
               PeekAhead(1)->GetType() == TokenType::IF)
        {
            Advance(); // consume if
            Advance(); // consume else
            ifStatement->nextElseIforElseStatements.push_back(
                ParseElseIfStatement());
        }

        if (Consume(TokenType::ELSE))
        {
            ifStatement->nextElseIforElseStatements.push_back(
                ParseElseStatement());
        }

        return ifStatement;
    }
    return ParseWhileLoopStatement();
}

Statement *Parser::ParseElseStatement()
{
    ElseStatement *elseStatement = gZtoonArena.Allocate<ElseStatement>();
    elseStatement->elseToken = Prev();
    elseStatement->blockStatement =
        dynamic_cast<BlockStatement *>(ParseBlockStatement());
    if (!dynamic_cast<BlockStatement *>(elseStatement->blockStatement))
    {
        ReportError("Else statement's block is missing",
                    elseStatement->GetCodeErrString());
    }

    return elseStatement;
}

Statement *Parser::ParseElseIfStatement()
{
    ElseIfStatement *elifStatement = gZtoonArena.Allocate<ElseIfStatement>();

    elifStatement->ifToken = Prev();
    elifStatement->expression = ParseExpression();
    if (!elifStatement->expression)
    {
        CodeErrString ces = {};

        ces.firstToken = Prev();
        ces.str = "else";

        ReportError("Expect expression after 'if else'", ces);
    }
    elifStatement->blockStatement =
        dynamic_cast<BlockStatement *>(ParseBlockStatement());

    if (!dynamic_cast<BlockStatement *>(elifStatement->blockStatement))
    {
        ReportError("Else if statement's block is missing",
                    elifStatement->expression->GetCodeErrString());
    }

    return elifStatement;
}

Statement *Parser::ParseWhileLoopStatement()
{
    if (Consume(TokenType::WHILE))
    {
        WhileLoopStatement *whileStatement =
            gZtoonArena.Allocate<WhileLoopStatement>();
        whileStatement->whileToken = Prev();
        whileStatement->condition = ParseExpression();
        if (!whileStatement->condition)
        {
            CodeErrString ces = {};
            ces.firstToken = whileStatement->whileToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expect expression after 'while'", ces);
        }

        whileStatement->blockStatement =
            dynamic_cast<BlockStatement *>(ParseBlockStatement());

        if (!dynamic_cast<BlockStatement *>(whileStatement->blockStatement))
        {
            CodeErrString ces;
            ces.firstToken = whileStatement->whileToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("while statement's block is missing"), ces);
        }

        return whileStatement;
    }

    return ParseForLoopStatement();
}

Statement *Parser::ParseForLoopStatement()
{
    if (Consume(TokenType::FOR))
    {
        ForLoopStatement *forLoopStatement =
            gZtoonArena.Allocate<ForLoopStatement>();
        forLoopStatement->forToken = Prev();
        bool leftParen = Consume(TokenType::LEFT_PAREN);
        forLoopStatement->init = ParseVarDeclStatement();
        if (!dynamic_cast<EmptyStatement *>(forLoopStatement->init))
        {
            if (!dynamic_cast<VarDeclStatement *>(forLoopStatement->init) &&
                !dynamic_cast<VarAssignmentStatement *>(
                    forLoopStatement->init) &&
                !dynamic_cast<VarCompoundAssignmentStatement *>(
                    forLoopStatement->init) &&
                !dynamic_cast<ExpressionStatement *>(forLoopStatement->init))
            {
                ReportError(
                    std::format(
                        "Initialization statement must be one of the "
                        "following types: Variable Declaration Statement"
                        "Variable Assignemnt Statement,Variable Compound "
                        "Assignemnt "
                        "Statement, Expression Statement."),
                    forLoopStatement->init->GetCodeErrString());
            }
        }

        if (!Consume(TokenType::SEMICOLON))
        {
            CodeErrString ces = {};
            ces.firstToken = Prev();
            ces.str = ces.firstToken->GetLexeme();
            ReportError(
                std::format("Expect ';' after expression '{}'",
                            forLoopStatement->init
                                ? forLoopStatement->init->GetCodeErrString().str
                                : ""),
                forLoopStatement->init
                    ? forLoopStatement->init->GetCodeErrString()
                    : ces);
        }
        forLoopStatement->condition = ParseExpression();

        if (!Consume(TokenType::SEMICOLON))
        {
            CodeErrString ces = {};
            ces.firstToken = Prev();
            ces.str = ces.firstToken->GetLexeme();
            ReportError(
                std::format(
                    "Expect ';' after expression '{}'",
                    forLoopStatement->condition
                        ? forLoopStatement->condition->GetCodeErrString().str
                        : ""),
                forLoopStatement->condition
                    ? forLoopStatement->condition->GetCodeErrString()
                    : ces);
        }
        forLoopStatement->update = ParseVarDeclStatement();
        if (leftParen)
        {
            if (!Consume(TokenType::RIGHT_PAREN))
            {
                CodeErrString ces;
                ces.firstToken = Prev();
                ces.str = ces.firstToken->GetLexeme();
                ReportError(std::format("Expected ')'"), ces);
            }
        }
        if (forLoopStatement->update)
        {
            if (!dynamic_cast<VarAssignmentStatement *>(
                    forLoopStatement->update) &&
                !dynamic_cast<VarCompoundAssignmentStatement *>(
                    forLoopStatement->update) &&
                !dynamic_cast<ExpressionStatement *>(forLoopStatement->update))
            {
                ReportError(
                    std::format("Update statement must be one of the "
                                "following types: "
                                "Variable Assignemnt Statement,Variable "
                                "Compound Assignemnt "
                                "Statement, Expression Statement."),
                    forLoopStatement->update->GetCodeErrString());
            }
        }
        forLoopStatement->blockStatement =
            dynamic_cast<BlockStatement *>(ParseBlockStatement());
        if (!dynamic_cast<BlockStatement *>(forLoopStatement->blockStatement))
        {
            CodeErrString ces = {};
            ces.firstToken = forLoopStatement->forToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("For loop's block is missing"), ces);
        }
        if (forLoopStatement->GetUpdate())
        {
            BlockStatement *updataBlockStatement =
                gZtoonArena.Allocate<BlockStatement>();
            updataBlockStatement->firstToken = Peek();
            updataBlockStatement->statements.push_back(
                forLoopStatement->update);
            forLoopStatement->update = updataBlockStatement;
        }
        BlockStatement *blockStatement = gZtoonArena.Allocate<BlockStatement>();
        blockStatement->firstToken = Peek();
        blockStatement->statements.push_back(forLoopStatement);

        return blockStatement;
    }

    return ParseBreakStatement();
}

Statement *Parser::ParseBreakStatement()
{
    if (Consume(TokenType::BREAK))
    {
        BreakStatement *bStmt = gZtoonArena.Allocate<BreakStatement>();
        bStmt->token = Prev();
        return bStmt;
    }

    return ParseContinueStatement();
}

Statement *Parser::ParseContinueStatement()
{
    if (Consume(TokenType::CONTINUE))
    {
        ContinueStatement *cStmt = gZtoonArena.Allocate<ContinueStatement>();
        cStmt->token = Prev();
        return cStmt;
    }
    return ParseRetStatement();
}

Statement *Parser::ParseRetStatement()
{
    if (Consume(TokenType::RET))
    {
        RetStatement *retStatement = gZtoonArena.Allocate<RetStatement>();
        retStatement->retToken = Prev();
        retStatement->expression = ParseExpression();

        return retStatement;
    }
    return ParseDeferStatement();
}

Statement *Parser::ParseDeferStatement()
{
    if (Consume(TokenType::DEFER))
    {
        DeferStatement *deferStmt = gZtoonArena.Allocate<DeferStatement>();
        deferStmt->token = Prev();
        deferStmt->statement = ParseStatement();
        if (!deferStmt->statement)
        {
            CodeErrString ces;
            ces.firstToken = deferStmt->token;
            ces.str = ces.firstToken->GetLexeme();
            ReportError("Expected statement after 'defer'", ces);
        }
        return deferStmt;
    }
    return ParseExpressionStatement();
}
Statement *Parser::ParseExpressionStatement()
{
    Expression *expr = ParseExpression();
    if (expr)
    {
        Statement *stmt = nullptr;
        stmt = ParseVarAssignmentStatement(expr);
        if (!stmt)
        {
            ExpressionStatement *es =
                gZtoonArena.Allocate<ExpressionStatement>();
            es->expression = expr;
            return es;
        }

        return stmt;
    }

    if (Peek()->GetType() == TokenType::SEMICOLON)
    {
        EmptyStatement *emtyStmt = gZtoonArena.Allocate<EmptyStatement>();
        return emtyStmt;
    }
    return nullptr;
}

Statement *Parser::ParseVarAssignmentStatement(Expression *lValueExpr)
{
    if (Consume(TokenType::EQUAL))
    {
        VarAssignmentStatement *statement =
            gZtoonArena.Allocate<VarAssignmentStatement>();
        statement->lValue = lValueExpr;
        statement->rValue = ParseExpression();
        return statement;
    }
    else
    {
        return ParseVarCompundAssignmentStatement(lValueExpr);
    }
}
Statement *Parser::ParseVarCompundAssignmentStatement(Expression *lValueExpr)
{
    if (IsCompoundAssignment(Peek()->GetType()))
    {
        Advance();
        VarCompoundAssignmentStatement *statement =
            gZtoonArena.Allocate<VarCompoundAssignmentStatement>();
        statement->lValue = lValueExpr;
        statement->compoundAssignment = Prev();
        Expression *left = lValueExpr;
        Expression *right = ParseExpression();
        if (!right)
        {
            CodeErrString ces = {};
            ces.firstToken = Peek();
            ces.str = " ";
            ReportError(std::format("Expect expression after '{}'",
                                    Peek()->GetLexeme()),
                        ces);
        }
        BinaryExpression *binaryExpression =
            gZtoonArena.Allocate<BinaryExpression>();
        binaryExpression->left = left;
        binaryExpression->right = right;

        if (!binaryExpression->right)
        {

            ReportError(
                std::format("Expect expression after '{}'",
                            binaryExpression->GetOperator()->GetLexeme()),
                binaryExpression->GetCodeErrString());
        }

        switch (statement->compoundAssignment->GetType())
        {
        case TokenType::PLUS_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::PLUS);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PLUS;
            op->lexeme = "+";
            binaryExpression->op = op;
            break;
        }
        case TokenType::DASH_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::DASH);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::DASH;
            op->lexeme = "-";
            binaryExpression->op = op;
            break;
        }
        case TokenType::ASTERISK_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::ASTERISK);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::ASTERISK;
            op->lexeme = "*";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SLASH_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SLASH);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SLASH;
            op->lexeme = "/";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_OR_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_OR);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::BITWISE_OR;
            op->lexeme = "|";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_AND_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_AND);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PLUS;
            op->lexeme = "&";
            binaryExpression->op = op;
            break;
        }
        case TokenType::BITWISE_XOR_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::BITWISE_XOR);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::BITWISE_XOR;
            op->lexeme = "^";
            binaryExpression->op = op;
            break;
        }
        case TokenType::PERCENTAGE_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::PERCENTAGE);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::PERCENTAGE;
            op->lexeme = "%%";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SHIFT_LEFT_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SHIFT_LEFT);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SHIFT_LEFT;
            op->lexeme = "<<";
            binaryExpression->op = op;
            break;
        }
        case TokenType::SHIFT_RIGHT_EQUAL:
        {
            Token *op = gZtoonArena.Allocate<Token>(TokenType::SHIFT_RIGHT);
            *op = *(statement->compoundAssignment);
            op->type = TokenType::SHIFT_RIGHT;
            op->lexeme = ">>";
            binaryExpression->op = op;
            break;
        }
        default:
        {
            break;
        }
        }
        statement->rValue = binaryExpression;
        return statement;
    }
    else
    {
        return nullptr;
    }
}
Expression *Parser::BuildBinaryExpression(Token const *op, Expression *left,
                                          Expression *right)
{
    BinaryExpression *expr = gZtoonArena.Allocate<BinaryExpression>();
    if (!left)
    {
        CodeErrString ces = {};
        ces.firstToken = op;
        ces.str = op->GetLexeme();
        ReportError(
            std::format("Expect expression before '{}'", op->GetLexeme()), ces);
    }

    if (!right)
    {
        CodeErrString ces = {};
        ces.firstToken = op;
        ces.str = op->GetLexeme();
        ReportError(
            std::format("Expect expression after '{}'", op->GetLexeme()), ces);
    }

    expr->left = left;
    expr->right = right;
    expr->op = op;
    return expr;
}

Expression *Parser::ParseExpression() { return ParseFnExpression(); }

Expression *Parser::ParseFnExpression()
{
    if (Consume(TokenType::FN))
    {
        FnExpression *fnExpr = gZtoonArena.Allocate<FnExpression>();
        fnExpr->fnToken = Prev();

        if (Consume(TokenType::LEFT_PAREN))
        {
            while (!Consume(TokenType::RIGHT_PAREN))
            {
                Statement *varDeclStatement = ParseVarDeclStatement();
                if (!dynamic_cast<VarDeclStatement *>(varDeclStatement))
                {
                    if (Consume(TokenType::VAR_ARGS))
                    {
                        fnExpr->isVarArgs = true;
                        if (Consume(TokenType::RIGHT_PAREN))
                        {
                            break;
                        }
                        else
                        {
                            CodeErrString ces;
                            ces.firstToken = Prev();
                            ces.str = ces.firstToken->GetLexeme();
                            ReportError(
                                std::format("'...' token can only be the last "
                                            "function paramter"),
                                ces);
                        }
                    }
                    else
                    {
                        ReportError(
                            std::format(
                                "Expect function paramter but found '{}'",
                                varDeclStatement->GetCodeErrString().str),
                            varDeclStatement->GetCodeErrString());
                    }
                }
                if (((VarDeclStatement *)varDeclStatement)->expression)
                {
                    ReportError("Default parameters are not supported",
                                varDeclStatement->GetCodeErrString());
                }
                ((VarDeclStatement *)varDeclStatement)->isParamter = true;
                fnExpr->parameters.push_back(
                    (VarDeclStatement *)varDeclStatement);
                Consume(TokenType::COMMA);
            }
        }
        else
        {
            CodeErrString ces;
            ces.firstToken = fnExpr->fnToken;
            ces.str = ces.firstToken->GetLexeme();
            ReportError(std::format("Expect '(' after '{}'", ces.str), ces);
        }

        if (Consume(TokenType::ARROW))
        {
            DataTypeToken *retDataType = ParseDataType();
            if (!retDataType)
            {
                CodeErrString ces = {};
                ces.firstToken = Prev();
                ces.str = ces.firstToken->GetLexeme();

                ReportError(std::format("Expect datatype after '->'", ces.str),
                            ces);
            }

            fnExpr->returnDataTypeToken = retDataType;
        }
        else
        {
            DataTypeToken *noType = gZtoonArena.Allocate<DataTypeToken>();
            noType->dataType = gZtoonArena.Allocate<Token>(TokenType::NOTYPE);
            fnExpr->returnDataTypeToken = noType;
        }

        if (Consume(TokenType::SEMICOLON))
        {
            fnExpr->isPrototype = true;
            fnExpr->blockStatement = gZtoonArena.Allocate<BlockStatement>();
            fnExpr->blockStatement->firstToken = Peek();
            for (Statement *s : fnExpr->parameters)
            {
                fnExpr->blockStatement->statements.push_back(s);
            }

            return fnExpr;
        }

        Statement *blockStatement = ParseBlockStatement();

        if (!dynamic_cast<BlockStatement *>(blockStatement))
        {
            ReportError(std::format("Expect block statement but found '{}'",
                                    blockStatement->GetCodeErrString().str),
                        blockStatement->GetCodeErrString());
        }

        fnExpr->blockStatement = (BlockStatement *)blockStatement;

        return fnExpr;
    }

    return ParseInitializerListExpression();
}

Expression *Parser::ParseInitializerListExpression()
{
    if (Consume(TokenType::LEFT_CURLY_BRACKET))
    {
        InitializerListExpression *initListExpr =
            gZtoonArena.Allocate<InitializerListExpression>();
        initListExpr->token = Prev();
        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {
            Expression *expr = ParseExpression();
            initListExpr->expressions.push_back(expr);
            Consume(TokenType::COMMA);
        }
        return initListExpr;
    }

    return ParseTernaryExpression();
}
Expression *Parser::ParseTernaryExpression()
{
    Expression *expr = ParseORExpression();

    if (Consume(TokenType::QUESTION_MARK))
    {
        TernaryExpression *ternaryExpr =
            gZtoonArena.Allocate<TernaryExpression>();
        ternaryExpr->questionMarkToken = Prev();
        ternaryExpr->trueExpr = ParseExpression();
        if (!ternaryExpr->trueExpr)
        {
            CodeErrString ces = {};
            ces.firstToken = ternaryExpr->questionMarkToken;
            ces.str = ternaryExpr->questionMarkToken->GetLexeme();
            ReportError("Expect an epxression after '?'", ces);
        }
        if (Consume(TokenType::COLON))
        {
            ternaryExpr->falseExpr = ParseExpression();

            if (!ternaryExpr->falseExpr)
            {

                CodeErrString ces = {};
                ces.firstToken = Prev();
                ces.str = Prev()->GetLexeme();
                ReportError("Expect an epxression after ':'", ces);
            }
        }
        else
        {
            ReportError("Expect ':' after expression",
                        ternaryExpr->condition->GetCodeErrString());
        }

        ternaryExpr->condition = expr;
        expr = ternaryExpr;
    }

    return expr;
}
Expression *Parser::ParseORExpression()
{
    Expression *expr = ParseANDExpression();
    while (Consume(TokenType::OR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseANDExpression());
    }
    return expr;
}
Expression *Parser::ParseANDExpression()
{
    Expression *expr = ParseBitwiseORExpression();
    while (Consume(TokenType::AND))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseORExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseORExpression()
{
    Expression *expr = ParseBitwiseXORExpression();
    while (Consume(TokenType::BITWISE_OR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseXORExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseXORExpression()
{
    Expression *expr = ParseBitwiseANDExpression();
    while (Consume(TokenType::BITWISE_XOR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseBitwiseANDExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseANDExpression()
{
    Expression *expr = ParseEqualEqualNotEqualExpression();
    while (Consume(TokenType::BITWISE_AND))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr,
                                     ParseEqualEqualNotEqualExpression());
    }
    return expr;
}
Expression *Parser::ParseEqualEqualNotEqualExpression()
{
    Expression *expr = ParseLessGreaterExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::EQUAL_EQUAL,
                      TokenType::EXCLAMATION_EQUAL))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseLessGreaterExpression());
    }
    return expr;
}
Expression *Parser::ParseLessGreaterExpression()
{
    Expression *expr = ParseShiftExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::LESS, TokenType::LESS_EQUAL,
                      TokenType::GREATER, TokenType::GREATER_EQUAL))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseLessGreaterExpression());
    }
    return expr;
}
Expression *Parser::ParseShiftExpression()
{
    Expression *expr = ParseTermExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::SHIFT_LEFT,
                      TokenType::SHIFT_RIGHT))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseTermExpression());
    }
    return expr;
}
Expression *Parser::ParseTermExpression()
{
    Expression *expr = ParseFactorExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::PLUS, TokenType::DASH))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseFactorExpression());
    }
    return expr;
}

Expression *Parser::ParseFactorExpression()
{
    Expression *expr = ParseCastExpression();

    while (TokenMatch(Peek()->GetType(), TokenType::ASTERISK, TokenType::SLASH,
                      TokenType::PERCENTAGE))
    {
        Advance();
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseCastExpression());
    }
    return expr;
}

Expression *Parser::ParseCastExpression()
{
    Expression *expr = ParseUnaryExpression();

    while (Consume(TokenType::AS))
    {
        CastExpression *castExpr = gZtoonArena.Allocate<CastExpression>();
        castExpr->expression = expr;
        DataTypeToken *castToDataType = ParseDataType();
        if (castToDataType)
        {
            castExpr->castToTypeToken = castToDataType;
        }
        else
        {
            CodeErrString ces = {};
            ces.firstToken = Prev();
            ces.str = Prev()->GetLexeme();
            ReportError("Expect datatype after 'as'", ces);
        }
        expr = castExpr;
    }

    return expr;
}
Expression *Parser::ParseUnaryExpression()
{
    Expression *expr = nullptr;
    if (TokenMatch(Peek()->GetType(), TokenType::ASTERISK,
                   TokenType::BITWISE_AND, TokenType::EXCLAMATION,
                   TokenType::TILDE))
    {
        Advance();
        UnaryExpression *unaryExpr = gZtoonArena.Allocate<UnaryExpression>();
        unaryExpr->op = Prev();
        unaryExpr->right = ParseUnaryExpression();

        expr = unaryExpr;
    }
    if (expr)
        return expr;

    if (TokenMatch(Peek()->GetType(), TokenType::DASH, TokenType::DASH_DASH,
                   TokenType::PLUS, TokenType::PLUS_PLUS, TokenType::SIZEOF))
    {
        Advance();
        UnaryExpression *unaryExpr = gZtoonArena.Allocate<UnaryExpression>();
        unaryExpr->op = Prev();
        if ((Prev()->GetType() == TokenType::SIZEOF) &&
            Consume(TokenType::LEFT_PAREN))
        {

            DataTypeToken *dataType = ParseDataType();
            auto placeHolderExpr = gZtoonArena.Allocate<PrimaryExpression>();
            placeHolderExpr->primary = Prev();
            unaryExpr->right = placeHolderExpr;
            unaryExpr->sizeOfDataTypeToken = dataType;
            if (!dataType)
            {
                CodeErrString ces = {};
                ces.firstToken = unaryExpr->op;
                ces.str = unaryExpr->op->GetLexeme();
                ReportError(std::format("Expect datatype after '{}'",
                                        unaryExpr->op->GetLexeme()),
                            ces);
            }

            if (Consume(TokenType::RIGHT_PAREN))
            {
                return unaryExpr;
            }
            else
            {
                ReportError(
                    "Expect ')' after expression.",
                    unaryExpr->GetRightExpression()->GetCodeErrString());
                return nullptr;
            }
        }

        unaryExpr->right = ParsePostfixExpression();

        if (TokenMatch(Peek()->GetType(), TokenType::DASH_DASH,
                       TokenType::PLUS_PLUS))
        {
            ReportError(std::format("Double unary expression are not allowd on "
                                    "the same expression '{}'",
                                    unaryExpr->right->GetCodeErrString().str),
                        unaryExpr->right->GetCodeErrString());
        }
        return unaryExpr;
    }

    expr = ParsePostfixExpression();
    if (TokenMatch(Peek()->GetType(), TokenType::DASH_DASH,
                   TokenType::PLUS_PLUS))
    {
        Advance();
        UnaryExpression *unaryExpr = gZtoonArena.Allocate<UnaryExpression>();
        unaryExpr->op = Prev();
        unaryExpr->right = expr;
        unaryExpr->postfix = true;
        return unaryExpr;
    }
    return expr;
}

Expression *Parser::ParsePostfixExpression()
{
    Expression *expr = ParsePrimaryExpression();
    while (TokenMatch(Peek()->GetType(), TokenType::PERIOD,
                      TokenType::DOUBLE_COLON, TokenType::LEFT_PAREN,
                      TokenType::LEFT_ANGLE_SQUARE,
                      TokenType::LEFT_SQUARE_BRACKET))
    {
        if (TokenMatch(Peek()->GetType(), TokenType::LEFT_PAREN,
                       TokenType::LEFT_ANGLE_SQUARE))
        {
            FnCallExpression *fnCallExpr =
                gZtoonArena.Allocate<FnCallExpression>();
            fnCallExpr->expression = expr;
            fnCallExpr->generic = ParseGeneric();

            Consume(TokenType::LEFT_PAREN);

            while (!Consume(TokenType::RIGHT_PAREN))
            {
                Expression *expr = ParseExpression();
                if (!expr)
                {
                    ReportError("Expect expression after '('",
                                expr->GetCodeErrString());
                }
                fnCallExpr->args.push_back(expr);
                Consume(TokenType::COMMA);
            }
            expr = fnCallExpr;
        }

        else if (Consume(TokenType::LEFT_SQUARE_BRACKET))
        {
            // subscript stuff.
            SubscriptExpression *subExpr =
                gZtoonArena.Allocate<SubscriptExpression>();
            subExpr->expression = expr;
            subExpr->token = Prev();
            subExpr->index = ParseExpression();
            subExpr->isLvalue = true;
            if (!Consume(TokenType::RIGHT_SQUARE_BRACKET))
            {
                ReportError("Missing ']'", subExpr->GetCodeErrString());
            }
            expr = subExpr;
        }

        else if (Consume(TokenType::PERIOD) || Consume(TokenType::DOUBLE_COLON))
        {
            const Token *op = Prev();
            PrimaryExpression *rightExpr =
                dynamic_cast<PrimaryExpression *>(ParsePrimaryExpression());
            if (rightExpr &&
                rightExpr->primary->GetType() != TokenType::IDENTIFIER)
            {
                CodeErrString ces;
                ces.firstToken = op;
                ces.str = op->GetLexeme();
                ReportError(std::format("Invalid expression after '{}'",
                                        op->GetLexeme()),
                            ces);
            }
            MemberAccessExpression *maExpr =
                gZtoonArena.Allocate<MemberAccessExpression>();
            maExpr->token = op;
            maExpr->isLvalue = true;
            maExpr->leftExpr = expr;
            maExpr->rightExpr = rightExpr;
            maExpr->accessType = MemberAccessExpression::AccessType::UNKNOWN;
            expr = maExpr;
        }
    }

    return expr;
}

Expression *Parser::ParsePrimaryExpression()
{
    Expression *retExpr = nullptr;
    switch (Peek()->GetType())
    {
    case TokenType::INTEGER_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::FLOAT_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::STRING_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::CHARACTER_LITERAL:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::TRUE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::FALSE:
    {
        Advance();
        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        break;
    }
    case TokenType::IDENTIFIER:
    {
        Token const *prev = Prev();
        Advance();

        PrimaryExpression *expr = gZtoonArena.Allocate<PrimaryExpression>();
        expr->primary = Prev();
        retExpr = expr;
        retExpr->isLvalue = true;
        break;
    }
    case TokenType::LEFT_PAREN:
    {
        Consume(TokenType::LEFT_PAREN);
        GroupingExpression *gExpr = gZtoonArena.Allocate<GroupingExpression>();
        gExpr->leftParen = Prev();
        gExpr->expression = ParseExpression();
        if (Consume(TokenType::RIGHT_PAREN))
        {
            retExpr = gExpr;
        }
        else
        {
            CodeErrString ces = {};
            ces.firstToken = Prev();
            ces.str = Prev()->GetLexeme();
            ReportError("Expected ')' after expression.", ces);
        }
        break;
    }
    case TokenType::NULL_PTR:
    {
        Consume(TokenType::NULL_PTR);
        PrimaryExpression *pExpr = gZtoonArena.Allocate<PrimaryExpression>();
        pExpr->primary = Prev();
        retExpr = pExpr;
    }
    break;
    default:
    {
        break;
    }
    }
    return retExpr;
}
