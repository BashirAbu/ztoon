#include "error_report.h"
#include "lexer/lexer.h"
#include "parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include <format>
#include <iostream>
std::unordered_map<Token const *, Token const *> identifierMap;

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

DataTypeToken *Parser::ParseDataType()
{
    DataTypeToken *dataType = gZtoonArena.Allocate<DataTypeToken>();
    if (Consume(TokenType::READONLY))
    {
        dataType->readOnly = Prev();
    }
    if (IsDataType(Peek()->GetType()))
    {
        Advance();
        dataType->dataType = Prev();
    }
    else
    {
        CodeErrString ces = {};
        ces.firstToken = dataType->readOnly;
        ces.str = ces.firstToken->GetLexeme();
        ReportError(std::format("Expect datatype after 'readonly'"), ces);
    }

    while (Consume(TokenType::ASTERISK))
    {
        auto ptrDesc = gZtoonArena.Allocate<DataTypeToken::PointerDesc>();
        auto ptrType = gZtoonArena.Allocate<DataTypeToken>();
        *ptrType = *dataType;
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
        dataType->readOnly = nullptr;
        dataType = ptrType;
    }
    return dataType;
}

bool Parser::IsDataType(TokenType type) { return ::IsDataType(type); }
Parser::Parser(const std::vector<Token *> &tokens) : tokens(tokens)
{
    currentStage = Stage::PARSER;
}

Parser::~Parser() {}

std::vector<Statement *> &Parser::Parse()
{
    while (Peek()->GetType() != TokenType::END_OF_FILE)
    {
        statements.push_back(ParseDeclaration());
    }
    return statements;
}

Statement *Parser::ParseDeclaration()
{
    // Parse VarDecl, FnStatment, later do struct, enum, union, ...
    Statement *declStmt = ParseStatement();

    if (dynamic_cast<VarDeclStatement *>(declStmt) ||
        dynamic_cast<FnStatement *>(declStmt))
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
                        "struct, enum, or union definition are allowed in "
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
        dynamic_cast<FnStatement *>(statement) || Consume(TokenType::SEMICOLON))
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

        while (!Consume(TokenType::RIGHT_CURLY_BRACKET))
        {
            blockStatement->statements.push_back(ParseStatement());
        }
        return blockStatement;
    }
    return ParseFnStatement();
}

Statement *Parser::ParseFnStatement()
{
    if (Consume(TokenType::FN))
    {
        FnStatement *fnStmt = gZtoonArena.Allocate<FnStatement>();
        fnStmt->fnToken = Prev();
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
                if (Peek()->GetType() != TokenType::RIGHT_PAREN)
                {
                    if (!Consume(TokenType::COMMA))
                    {
                        ReportError(
                            std::format(
                                "Expect ',' after function paramter '{}'",
                                varDeclStatement->GetCodeErrString().str),
                            varDeclStatement->GetCodeErrString());
                    }
                }
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

        return fnStmt;
    }
    return ParseVarDeclStatement();
}
Statement *Parser::ParseVarDeclStatement()
{
    if (Peek()->GetType() == TokenType::IDENTIFIER &&
        PeekAhead(1)->GetType() == TokenType::COLON)
    {
        Advance();
        VarDeclStatement *varDeclStatement =
            gZtoonArena.Allocate<VarDeclStatement>();
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
                return varDeclStatement;
            }
            else
            {
                CodeErrString ces = {};
                ces.firstToken = varDeclStatement->GetIdentifier();
                ces.str = ces.firstToken->GetLexeme();
                ReportError("Expected datatype after ':'", ces);
            }
        }
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
        Consume(TokenType::LEFT_PAREN);
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
        Consume(TokenType::RIGHT_PAREN);
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
            updataBlockStatement->statements.push_back(
                forLoopStatement->update);
            forLoopStatement->update = updataBlockStatement;
        }
        BlockStatement *blockStatement = gZtoonArena.Allocate<BlockStatement>();
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

Expression *Parser::ParseExpression()
{
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
            if (!Consume(TokenType::COMMA))
            {
                if (Peek()->type == TokenType::RIGHT_CURLY_BRACKET)
                {
                    continue;
                }
                ReportError("Expected ',' after expression",
                            expr->GetCodeErrString());
            }
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
    }
    return expr;
}
Expression *Parser::ParseANDExpression()
{
    Expression *expr = ParseBitwiseORExpression();
    while (Consume(TokenType::AND))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseORExpression()
{
    Expression *expr = ParseBitwiseXORExpression();
    while (Consume(TokenType::BITWISE_OR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseExpression());
    }
    return expr;
}
Expression *Parser::ParseBitwiseXORExpression()
{
    Expression *expr = ParseBitwiseANDExpression();
    while (Consume(TokenType::BITWISE_XOR))
    {
        Token const *op = Prev();
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
        expr = BuildBinaryExpression(op, expr, ParseExpression());
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
    if (TokenMatch(Peek()->GetType(), TokenType::DASH, TokenType::DASH_DASH,
                   TokenType::PLUS, TokenType::PLUS_PLUS,
                   TokenType::EXCLAMATION, TokenType::TILDE, TokenType::SIZEOF,
                   TokenType::ASTERISK, TokenType::BITWISE_AND))
    {
        Advance();
        UnaryExpression *unaryExpr = gZtoonArena.Allocate<UnaryExpression>();
        unaryExpr->op = Prev();
        if ((Prev()->GetType() == TokenType::SIZEOF) &&
            Consume(TokenType::LEFT_PAREN))
        {
            unaryExpr->right = ParseExpression();
            if (!unaryExpr->right)
            {
                DataTypeToken *dataType = ParseDataType();
                if (!dataType)
                {
                    CodeErrString ces = {};
                    ces.firstToken = unaryExpr->op;
                    ces.str = unaryExpr->op->GetLexeme();
                    ReportError(std::format("Expect expression after '{}'",
                                            unaryExpr->op->GetLexeme()),
                                ces);
                }

                unaryExpr->sizeOfDataTypeToken = dataType;
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
        unaryExpr->right = ParseExpression();

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

    Expression *expr = ParsePostfixExpression();
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

    if (Consume(TokenType::LEFT_PAREN))
    {
        FnCallExpression *fnCallExpr = gZtoonArena.Allocate<FnCallExpression>();
        fnCallExpr->expression = expr;
        while (!Consume(TokenType::RIGHT_PAREN))
        {
            Expression *expr = ParseExpression();
            if (!expr)
            {
                ReportError("Expect expression after '('",
                            expr->GetCodeErrString());
            }
            fnCallExpr->args.push_back(expr);
            if (!Consume(TokenType::COMMA))
            {
                if (Peek()->GetType() == TokenType::RIGHT_PAREN)
                {
                    continue;
                }
                else
                {
                    ReportError(std::format("Expect ',' after argument '{}'",
                                            expr->GetCodeErrString().str),
                                expr->GetCodeErrString());
                }
            }
        }
        return fnCallExpr;
    }

    while (Consume(TokenType::LEFT_SQUARE_BRACKET))
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
    default:
    {
        break;
    }
    }
    return retExpr;
}

// void Parser::PrettyPrintAST()
// {
//     for (Statement *s : statements)
//     {
//         std::string prefix = "";
//         std::string astStr = s->PrettyString(prefix);
//         std::cout << astStr << std::endl;
//     }
// }
// std::string BlockStatement::PrettyString(std::string &prefix)
// {

//     bool isLeft = prefix.empty() ? false : *(prefix.end() - 1) == '|';
//     if (isLeft)
//         prefix.pop_back();
//     std::string str = prefix;
//     str += isLeft ? "|-" : "|_";
//     str += "(Block)\n";
//     prefix += isLeft ? "|   " : "    ";
//     for (auto s : statements)
//     {
//         str += s->PrettyString(prefix);
//     }
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string IfStatement::PrettyString(std::string &prefix)
// {
//     std::string str = prefix;
//     str += "|_(if)\n";
//     prefix += "|   |";
//     str += blockStatement->PrettyString(prefix);
//     str += expression->PrettyString(prefix, false);

//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     for (auto s : nextElseIforElseStatements)
//     {
//         str += s->PrettyString(prefix);
//     }
//     return str;
// }

// std::string ElseIfStatement::PrettyString(std::string &prefix)
// {

//     std::string str = prefix;
//     str += "|_(else if)\n";
//     prefix += "|   |";
//     str += blockStatement->PrettyString(prefix);
//     str += expression->PrettyString(prefix, false);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string ElseStatement::PrettyString(std::string &prefix)
// {

//     std::string str = prefix;
//     str += "|_(else)\n";
//     prefix += "    ";
//     str += blockStatement->PrettyString(prefix);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string VarDeclStatement::PrettyString(std::string &prefix)
// {
//     std::string str = prefix;
//     str += "|_(=)\n";
//     prefix += "    ";
//     if (expression)
//     {
//         str += expression->PrettyString(prefix, true);
//     }
//     str += prefix;
//     str += "|_";
//     str += "var_decl(" + identifier->GetLexeme() + " (" +
//            dataTypeToken->ToString() + ")" + ")\n";
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string VarAssignmentStatement::PrettyString(std::string &prefix)
// {
//     std::string str = prefix;
//     str += "|_(=)\n";
//     prefix += "    ";
//     str += expression ? expression->PrettyString(prefix, true) : "";
//     str += prefix;
//     str += "|_";
//     str += "var_assign(" + identifier->GetLexeme() + ")\n";
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string VarCompoundAssignmentStatement::PrettyString(std::string &prefix)
// {
//     std::string str = prefix;
//     str += "|_(=)\n";
//     prefix += "    ";
//     str += expression->PrettyString(prefix, true);
//     str += prefix;
//     str += "|_";
//     str += "var_assign(" + identifier->GetLexeme() + " (datatype" + ")" +
//     ")\n"; prefix.pop_back(); prefix.pop_back(); prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string WhileLoopStatement::PrettyString(std::string &prefix)
// {

//     std::string str = prefix;
//     str += "|_(while)\n";
//     prefix += "    ";
//     str += condition->PrettyString(prefix, true);
//     str += blockStatement->PrettyString(prefix);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string ForLoopStatement::PrettyString(std::string &prefix)
// {

//     std::string str = prefix;
//     str += "|_(for)\n";
//     prefix += "    ";
//     str += init ? init->PrettyString(prefix) : prefix + "|-\n";
//     str += prefix;

//     str += condition ? condition->PrettyString(prefix, true)
//                      : prefix + "|-true(empty)\n";

//     str += prefix;
//     str += update ? update->PrettyString(prefix) : prefix + "|-\n";
//     str += blockStatement->PrettyString(prefix);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }
// std::string FnStatement::PrettyString(std::string &prefix)
// {
//     std::string str = "";
//     str += prefix;
//     str = "|_";
//     str += (std::string) "(Lambda)\n";
//     prefix += "    ";
//     for (Statement *s : parameters)
//     {
//         str += s->PrettyString(prefix);
//     }
//     str += prefix + "|_ret_type(" +
//            (returnDataTypeToken ? returnDataTypeToken->ToString() : "NoRet ")
//            +
//            ")\n";
//     if (blockStatement)
//         str += blockStatement->PrettyString(prefix);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }
// std::string RetStatement::PrettyString(std::string &prefix)
// {
//     std::string str = prefix + "ret\n";
//     prefix += "    ";
//     str += expression->PrettyString(prefix, false);
//     return str;
// }

// std::string ExpressionStatement::PrettyString(std::string &prefix)
// {
//     std::string str = "";
//     str += expression ? expression->PrettyString(prefix, false) : "";
//     return str;
// }

// std::string FnCallExpression::PrettyString(std::string &prefix, bool isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += (std::string) "FnCall(" + identifier->GetLexeme() + ")\n";
//     prefix += isLeft ? "|   " : "    ";
//     for (Expression *s : args)
//     {
//         str += s->PrettyString(prefix, true);
//     }
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string TernaryExpression::PrettyString(std::string &prefix, bool isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += (std::string) "Ternary\n";
//     prefix += isLeft ? "|   " : "    ";
//     str += condition->PrettyString(prefix, true);
//     str += trueExpr->PrettyString(prefix, true);
//     str += falseExpr->PrettyString(prefix, false);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string BinaryExpression::PrettyString(std::string &prefix, bool isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += (std::string) "(" + op->GetLexeme() + ")\n";
//     prefix += isLeft ? "|   " : "    ";
//     str += left->PrettyString(prefix, true);
//     str += right->PrettyString(prefix, false);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string UnaryExpression::PrettyString(std::string &prefix, bool isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += (std::string) "(" + op->GetLexeme() + ")\n";
//     prefix += isLeft ? "|   " : "    ";
//     str += right->PrettyString(prefix, false);
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();

//     return str;
// }

// std::string GroupingExpression::PrettyString(std::string &prefix, bool
// isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += "()\n";
//     prefix += isLeft ? "|   " : "    ";
//     str += expression ? expression->PrettyString(prefix, false) : "";
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string CastExpression::PrettyString(std::string &prefix, bool isLeft)
// {
//     std::string str = "";
//     str += prefix;
//     str += isLeft ? "|-" : "|_";
//     str += (std::string) "as" + "\n";
//     prefix += isLeft ? "|   " : "    ";
//     str += prefix;
//     str += "|-";
//     str += castToTypeToken->ToString() + "\n";
//     str += expression ? expression->PrettyString(prefix, true) : "";
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     prefix.pop_back();
//     return str;
// }

// std::string PrimaryExpression::PrettyString(std::string &prefix, bool isleft)
// {
//     std::string str = "";

//     str += prefix;
//     str += isleft ? "|-" : "|_";
//     str += primary->GetLexeme() + "\n";
//     return str;
// }
