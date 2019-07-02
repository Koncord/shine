#include "parser.hpp"

#include <lexer/token.hpp>
#include <lexer/lexer.hpp>
#include <utils/exception.hpp>
#include <algorithm>

using namespace shine;


#ifdef DEBUG_PARSER

void Parser::debug(std::string_view str)
{
    coloredPrintf(Color::Gray, str);
    lexer->inspect();
}

#else

void Parser::debug(std::string_view str) {}

#endif

bool Parser::accept(TokenType t)
{
    if (lexer->isType(t))
    {
        lexer->getNextToken();
        return true;
    }
    return false;
}

void Parser::expect(TokenType t, const std::string &error)
{
    if (!lexer->isType(t))
        throw ParseException(this, error);
}

void Parser::expectNext(TokenType t, const std::string &error)
{
    if (!accept(t))
        throw ParseException(this, error);
}

const node::NodePtr &Parser::expect(const node::NodePtr &node, const std::string &error)
{
    if (node == nullptr)
        throw ParseException(this, error);
    return node;
}

/*
 * '(' expression ')'
 */

node::NodePtr Parser::paren_expression()
{
    node::NodePtr node;
    debug("paren_expression");
    if (!accept(TokenType::LParen)) return nullptr;
    if ((node = expression()) == nullptr) return nullptr;

    expectNext(TokenType::RParen, "expression missing closing ')'");

    return node;
}

/*
 *   expression ','?
 * | expr ',' arg_list
 */

bool Parser::arg_list(std::vector<node::NodePtr> &vals, TokenType delim)
{
    // trailing ','
    if (lexer->isType(delim)) return true;

    // expression
    if (node::NodePtr val = expression(); val)
        vals.emplace_back(val);
    else
        return false;

    // ',' arg_list
    if (accept(TokenType::Comma))
    {
        if (!arg_list(vals, delim)) return false;
    }

    return true;
}

/*
 * '[' arg_list? ']'
 */

node::NodePtr Parser::array_expression()
{
    auto node = std::make_shared<node::Array>(lexer->getPos());
    node->pos.linepos -= 1;
    debug("array_expression");

    if (!accept(TokenType::LBrack)) return nullptr;

    ctx = "array expression";
    if (!arg_list(node->vals, TokenType::RBrack)) return nullptr;
    expectNext(TokenType::RBrack, "expression missing closing ']'");

    return node;
}

/*
 *   id ':' expr
 * | id ':' expression ',' hash_pairs
 */

bool Parser::hash_pairs(std::vector<node::HashPairPtr> &pairs, TokenType delim)
{
    // trailing ','
    if (lexer->isType(delim)) return true;

    auto pair = std::make_shared<node::HashPair>(lexer->getPos());
    if ((pair->key = expression()) == nullptr)
        return false;

    // :
    expectNext(TokenType::Colon, "hash pair ':' missing");


    // expression
    if ((pair->val = expression()) == nullptr)
        return false;

    pairs.emplace_back(pair);

    // ',' hash_pairs
    if (accept(TokenType::Comma))
    {
        if (!hash_pairs(pairs, delim)) return false;
    }

    return true;
}

/*
 * '{' hash_pairs? '}'
 */

node::NodePtr Parser::hash_expression()
{
    auto node = std::make_shared<node::Hash>(lexer->getPos());
    debug("hash_expression");

    if (!accept(TokenType::LBrace)) return nullptr;
    ctx = "hash expression";
    if (!hash_pairs(node->pairs, TokenType::RBrace)) return nullptr;
    expectNext(TokenType::RBrace, "hash missing closing '}'");
    return node;
}

/*
 * type ('[' expression ']')?
 */

node::NodePtr Parser::type_expression()
{
    debug("type_expression");
    int ptrLevel = 0;
    while(accept(TokenType::OpMul))
        ++ptrLevel;
    if (!lexer->isType(TokenType::Id)) return nullptr;

    auto ret = std::make_shared<node::Type>(std::get<std::string>(lexer->getToken().value), ptrLevel, lexer->getPos());

    lexer->getNextToken();
    ret->isArray = accept(TokenType::LBrack);

    if(ret->isArray)
    {
        //expect(TokenType::I64, "expected number");
        if (lexer->isType(TokenType::I64))
        {
            ret->arrSize = std::get<int64_t>(lexer->getToken().value);
            lexer->getNextToken();
        }
        expectNext(TokenType::RBrack, "type missing closing ']'");
    }

    if (accept(TokenType::LParen))
    {
        ret->isFunc = true;
        ret->funParams = function_params();
        expectNext(TokenType::RParen, "type missing closing ')'");
    }

    return ret;
}

/*
 * id (',' id)* ':' type_expression
 */

node::NodePtr Parser::decl_expression(bool needType)
{
    debug("decl_expression");
    ctx = "declaration expression";

    if (!lexer->isType(TokenType::Id)) return nullptr;

    std::vector<node::IdPtr> vec;
    auto pos = lexer->getPos();
    while (lexer->isType(TokenType::Id))
    {
        // id
        vec.emplace_back(getId());

        // ','
        if (!accept(TokenType::Comma)) break;
    }

    // ':'
    if (!accept(TokenType::Colon))
    {
        if (needType)
            throw ParseException(this, "expecting type");
        return std::make_shared<node::Decl>(vec, nullptr, pos);
    }

    node::TypePtr type = expect(type_expression(), "expecting type")->as<node::Type>();

    return std::make_shared<node::Decl>(vec, type, pos);
}

/*
 *   id
 * | int
 * | float
 * | string
 * | array
 * | hash
 * | paren_expression
 */

node::NodePtr Parser::primary_expression()
{
    debug("primary_expression");
    node::NodePtr ret = nullptr;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wswitch"
    switch (lexer->getToken().type)
    {
        case TokenType::Id:
            ret = std::make_shared<node::Id>(std::get<std::string>(lexer->getToken().value), lexer->getPos());
            break;
        case TokenType::I64:
        case TokenType::I32:
        case TokenType::I16:
        case TokenType::I8:
        case TokenType::U64:
        case TokenType::U32:
        case TokenType::U16:
        case TokenType::U8:
            ret = std::make_shared<node::Int>(std::get<int64_t>(lexer->getToken().value), 8, lexer->getPos());
            break;
        case TokenType::Boolean:
            ret = std::make_shared<node::Boolean>(std::get<bool>(lexer->getToken().value), lexer->getPos());
            break;
        case TokenType::Float:
            ret = std::make_shared<node::Float>(std::get<double>(lexer->getToken().value), lexer->getPos());
            break;
        case TokenType::String:
            ret = std::make_shared<node::String>(std::get<std::string>(lexer->getToken().value), lexer->getPos());
            break;
        case TokenType::LBrack:
            return array_expression();
        case TokenType::LBrace:
            return hash_expression();
    }
#pragma clang diagnostic pop
    if (ret)
    {
        lexer->getNextToken();
        return ret;
    }
    return paren_expression();
}

/*
 *   call_expression
 * | call_expression '++'
 * | call_expression '--'
 */

node::NodePtr Parser::postfix_expression()
{
    node::NodePtr node;
    auto pos = lexer->getPos();
    debug("postfix_expression");
    if ((node = call_expression(nullptr)) == nullptr) return nullptr;
    if (lexer->isType(TokenType::OpIncr) || lexer->isType(TokenType::OpDecr))
    {
        node = std::make_shared<node::UnaryOp>(lexer->getToken().type, node, true, pos);

        lexer->getNextToken();
    }
    return node;
}

/*
 *   '++' unary_expression
 * | '--' unary_expression
 * | '~' unary_expression
 * | '!' unary_expression
 * | '+' unary_expression
 * | '-' unary_expression
 * | '&' unary_expression
 * | '*' unary_expression
 * | 'sizeof' '(' unary_expression ')'
 * | postfix_expression
 */

node::NodePtr Parser::unary_expression()
{
    debug("unary_expression");
    auto pos = lexer->getPos();
    if (lexer->isType(TokenType::OpIncr) || lexer->isType(TokenType::OpDecr)
        || lexer->isType(TokenType::OpBitNot) || lexer->isType(TokenType::OpNot)
        || lexer->isType(TokenType::OpPlus) || lexer->isType(TokenType::OpMinus)
        || lexer->isType(TokenType::OpBitAnd) || lexer->isType(TokenType::OpMul))
    {
        TokenType op = lexer->getToken().type;

        lexer->getNextToken();
        return std::make_shared<node::UnaryOp>(op, unary_expression(), false, pos);
    }
    else if (accept(TokenType::SizeOf))
    {
        expectNext(TokenType::LParen, "type missing closing '('");
        auto expr = std::make_shared<node::UnaryOp>(TokenType::SizeOf, unary_expression(), false, pos);
        expectNext(TokenType::RParen, "type missing closing ')'");
        return expr;

    }
    return postfix_expression();
}

/*
 * unary_expr (('* | '/' | '%') unary_expression)*
 */

node::NodePtr Parser::multiplicative_expression()
{
    TokenType op;
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("multiplicative_expression");
    if ((node = unary_expression()) == nullptr) return nullptr;
    while (lexer->isType(TokenType::OpMul) || lexer->isType(TokenType::OpDiv) || lexer->isType(TokenType::OpMod))
    {
        op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "multiplicative operation expression";
        right = expect(unary_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(op, node, right, pos);
    }
    return node;
}

/*
 * multiplicative_expr (('+ | '-') multiplicative_expression)*
 */

node::NodePtr Parser::additive_expression()
{
    TokenType op;
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("additive_expression");
    if ((node = multiplicative_expression()) == nullptr) return nullptr;
    while (lexer->isType(TokenType::OpPlus) || lexer->isType(TokenType::OpMinus))
    {
        op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "additive operation expression";
        right = expect(multiplicative_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(op, node, right, pos);
    }
    return node;
}

/*
 * additive_expr (('<<' | '>>') additive_expression)*
 */

node::NodePtr Parser::shift_expression()
{
    TokenType op;
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("shift_expression");
    if ((node = additive_expression()) == nullptr) return nullptr;
    while (lexer->isType(TokenType::OpBitShL) || lexer->isType(TokenType::OpBitShR))
    {
        op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "shift operation expression";
        right = expect(additive_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(op, node, right, pos);
    }
    return node;
}

/*
 * shift_expr (('<' | '<=' | '>' | '>=') shift_expression)*
 */

node::NodePtr Parser::relational_expression()
{
    TokenType op;
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("relational_expression");
    if ((node = shift_expression()) == nullptr) return nullptr;
    while(lexer->isRelational())
    {
        op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "relational operation expression";
        right = expect(shift_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(op, node, right, pos);
    }
    return node;
}

/*
 * relational_expr (('==' | '!=') relational_expression)*
 */

node::NodePtr Parser::equality_expression()
{
    TokenType op;
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("equality_expression");
    if ((node = relational_expression()) == nullptr) return nullptr;
    while (lexer->isEquality())
    {
        op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "equality operation expression";
        right = expect(relational_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(op, node, right, pos);
    }
    return node;
}

/*
 * equality_expr ('and' equality_expression)*
 */

node::NodePtr Parser::bitwise_and_expression()
{
    node::NodePtr node, right;
    debug("bitwise_and_expression");
    auto pos = lexer->getPos();
    if ((node = equality_expression()) == nullptr) return nullptr;
    while (accept(TokenType::OpBitAnd))
    {
        ctx = "& operation expression";
        right = expect(equality_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(TokenType::OpBitAnd, node, right, pos);
    }
    return node;
}

/*
 * bitwise_and_expr ('^' bitwise_and_expression)*
 */

node::NodePtr Parser::bitwise_xor_expression()
{
    node::NodePtr node, right;
    debug("bitwise_xor_expression");
    auto pos = lexer->getPos();
    if ((node = bitwise_and_expression()) == nullptr) return nullptr;
    while (accept(TokenType::OpBitXor))
    {
        ctx = "^ operation expression";
        right = expect(bitwise_and_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(TokenType::OpBitXor, node, right, pos);
    }
    return node;
}

/*
 * bitwise_xor_expr ('|' bitwise_xor_expression)*
 */

node::NodePtr Parser::bitswise_or_expression()
{
    node::NodePtr node, right;
    debug("bitswise_or_expression");
    auto pos = lexer->getPos();
    if ((node = bitwise_xor_expression()) == nullptr) return nullptr;
    while (accept(TokenType::OpBitOr))
    {
        ctx = "| operation expression";
        right = expect(bitwise_xor_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(TokenType::OpBitOr, node, right, pos);
    }
    return node;
}

/*
 * bitswise_or_expr ('&&' bitswise_or_expression)*
 */

node::NodePtr Parser::logical_and_expression()
{
    node::NodePtr node, right;
    debug("logical_and_expression");
    auto pos = lexer->getPos();
    if ((node = bitswise_or_expression()) == nullptr) return nullptr;
    while (accept(TokenType::OpAnd))
    {
        ctx = "&& operation expression";
        right = expect(bitswise_or_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(TokenType::OpAnd, node, right, pos);
    }
    return node;
}

/*
 * logical_and_expr ('||' logical_and_expression)*
 */

node::NodePtr Parser::logical_or_expression()
{
    node::NodePtr node, right;
    auto pos = lexer->getPos();
    debug("logical_or_expression");
    if ((node = logical_and_expression()) == nullptr) return nullptr;

    // '||'
    while (accept(TokenType::OpOr))
    {
        ctx = "|| operation expression";
        right = expect(logical_and_expression(), "missing right-hand expression");

        node = std::make_shared<node::BinaryOp>(TokenType::OpOr, node, right, pos);
    }

    return node;
}

/*
 * (decl_expr ('=' expr)? (',' decl_expression ('=' expression)?)*)
 */

std::vector<node::NodePtr> Parser::function_params()
{
    std::vector<node::NodePtr> params;
    debug("params");
    ctx = "function params";

    if (!lexer->isType(TokenType::Id)) return params;

    do
    {
        auto pos = lexer->getPos();
        node::DeclPtr decl;
        if (auto expr = decl_expression(false); expr)
            decl = expr->as<node::Decl>();
        else if (lexer->isType(TokenType::VaArg))
        {
            params.push_back(std::make_shared<node::VaArg>(pos));
            lexer->getNextToken();
            break;
        }
        else
            return params;

        ctx = "function param";

        // ('=' expression)?
        if (accept(TokenType::OpAssign))
        {
            node::NodePtr val = expression();
            if (!val) return params;
            auto node = std::make_shared<node::BinaryOp>(TokenType::OpAssign, decl, val, pos);
            params.push_back(node);
        }
        else
        {
            expect(decl->type, "expecting type");
            params.push_back(decl);
        }

    } while (accept(TokenType::Comma));

    return params;
}

/*
 *   primary_expression
 * | primary_expr '[' expression ']'
 * | primary_expr '.' id
 * | primary_expression '.' call_expression
 */

node::NodePtr Parser::slot_access_expression(node::NodePtr left)
{
    auto pos = lexer->getPos();
    debug("slot_access_expression");

    // primary_expression
    if (!left)
    {
        left = primary_expression();
        if (!left) return nullptr;
    }

    // subscript
    if (accept(TokenType::LBrack))
    {
        node::NodePtr right;

        right = expect(expression(), "missing index in subscript");

        ctx = "subscript expression";
        expectNext(TokenType::RBrack, "missing closing ']'");
        auto c = std::make_shared<node::Subscript>(
                left,
                right,
                pos);
        return call_expression(c);
    }

    if (accept(TokenType::OpModScope))
    {
        ctx = "scope expression";
        std::vector<std::string> scope;
        scope.push_back(left->as<node::Id>()->val);
        do
        {
            auto id = getId()->val;
            scope.push_back(id);
        } while (accept(TokenType::OpModScope));
        left = std::make_shared<node::Scope>(scope, pos);
    }

    // slot
    while (accept(TokenType::OpDot))
    {
        ctx = "slot access expression";

        auto id = getId();

        if (lexer->isType(TokenType::LParen))
        {
            std::vector<node::NodePtr> args_vec;
            node::NodePtr prev;

            auto call = std::static_pointer_cast<node::Call>(call_expression(id));
            if (!call->args->vec.empty())
            {
                // re-organize call arguments
                call->args->vec.push_back(left);
                std::rotate(call->args->vec.begin(), call->args->vec.begin() + 1, call->args->vec.end());
                args_vec =  call->args->vec;
            }
            else
            {
                prev = left;
                args_vec = call->args->vec;
            }

            // add last argument
            if (prev != nullptr)
                args_vec.push_back(prev);

            call->args->vec = args_vec;
            left = call;

        }
        else
            left = std::make_shared<node::Slot>(left, id, pos);

        left = call_expression(left);
    }

    return left;
}

/*
 * (expr (',' expression)*)
 */

node::ArgsPtr Parser::call_args()
{
    node::NodePtr node;
    auto args = std::make_shared<node::Args>(lexer->getPos());

    debug("args");
    do
    {
        auto copyCtx = ctx;
        if ((node = expression()) == nullptr)
            return nullptr;

        if (node->is(NodeType::Array) || node->is(NodeType::Hash))
        {
            ctx = copyCtx;
            throw ParseException(this, node->pos, "expected expression");
        }

        if (accept(TokenType::Colon))
        {
            ctx = "keyword argument";

            if (node->nodeType == NodeType::String || node->nodeType == NodeType::Id)
            {
                node::NodePtr val = expression();
                std::string str = std::static_pointer_cast<node::Id>(node)->val;
                args->hash.emplace(str, val);
            }
            else
                throw ParseException(this, "expecting string or identifier as key");
        }
        else
            args->vec.emplace_back(node);

    } while (accept(TokenType::Comma));

    return args;
}

/*
 *   slot_access_expr '(' args? ')'
 * | slot_access_expression
 */

node::NodePtr Parser::call_expression(node::NodePtr left)
{
    //node::NodePtr right;
    node::NodePtr prev = left;
    node::CallPtr call = nullptr;
    auto pos = lexer->getPos();
    debug("call_expression");

    // slot_access_expression
    if (!left)
    {
        if ((left = slot_access_expression(nullptr)) == nullptr) return nullptr;
    }

    // '('
    if (accept(TokenType::LParen))
    {
        ctx = "function call expression";
        call = std::make_shared<node::Call>(left, pos);

        // args? ')'
        if (!lexer->isType(TokenType::RParen))
        {
            call->args = call_args();
            expect(TokenType::RParen, "missing closing ')'");
        }

        lexer->getNextToken();
        left = call;
    }

    if (lexer->isType(TokenType::OpDot) && prev)
    {
        // stop here if the there was a previous left-hand expression
        // and the current token is '.' because we're
        // probably inside the loop in slot_access_expression
        return left;
    }
    else if (lexer->isType(TokenType::LParen))
        return call_expression(left);
    return slot_access_expression(left);
}

/*
 * ('let' | 'const') decl_expr ('=' expression)? (',' decl_expression ('=' expr)?)*
 */

node::NodePtr Parser::variable_expression(const TokenType &tokenType)
{
    // let already consumed
    std::vector<node::BinaryOpPtr> vec;
    auto let_pos = lexer->getPos();

    do
    {
        if (tokenType == TokenType::Let)
            ctx = "let expression";
        else
            ctx = "const expression";

        auto pos = lexer->getPos();
        bool needType = true; // todo: inference type by value
        node::NodePtr decl = expect(decl_expression(needType), "expecting declaration");
        node::NodePtr val = nullptr;

        // '='
        if (accept(TokenType::OpAssign))
            val = expect(expression(), "expecting declaration initializer");

        auto bin = std::make_shared<node::BinaryOp>(TokenType::OpAssign, decl, val, pos);
        vec.emplace_back(bin);
    } while (accept(TokenType::Comma));

    if (tokenType == TokenType::Let)
        return std::make_shared<node::Let>(vec, let_pos);
    else
        return std::make_shared<node::Const>(vec, let_pos);
}

/*
 *   logical_or_expression
 * | variable_expression
 * | call_expression '=' not_expression
 * | call_expression '+=' not_expression
 * | call_expression '-=' not_expression
 * | call_expression '/=' not_expression
 * | call_expression '*=' not_expression
 * | call_expression '|=' not_expression
 * | call_expression '&=' not_expression
 */

node::NodePtr Parser::assignment_expression()
{
    node::NodePtr node, right;
    auto pos = lexer->getPos();

    TokenType curTokType = lexer->getToken().type;
    // variable_expression?
    if (accept(TokenType::Let) || accept(TokenType::Const)) return variable_expression(curTokType);

    debug("assignment_expression");
    if ((node = logical_or_expression()) == nullptr) return nullptr;

    // =
    if (lexer->isType(TokenType::OpAssign))
    {
        TokenType op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "assignment expression";
        if ((right = not_expression()) == nullptr) return nullptr;
        return std::make_shared<node::BinaryOp>(op, node, right, pos);
    }

    // compound
    if (lexer->isType(TokenType::OpPlusAssign)
        || lexer->isType(TokenType::OpMinusAssign)
        || lexer->isType(TokenType::OpDivAssign)
        || lexer->isType(TokenType::OpMulAssign)
        || lexer->isType(TokenType::OpOrAssign)
        || lexer->isType(TokenType::OpAndAssign))
    {
        TokenType op = lexer->getToken().type;

        lexer->getNextToken();
        ctx = "compound assignment expression";
        if ((right = not_expression()) == nullptr) return nullptr;
        return std::make_shared<node::BinaryOp>(op, node, right, pos);
    }

    return node;
}

/*
 *   'not' not_expression
 * | assignment_expression
 */

node::NodePtr Parser::not_expression()
{
    auto pos = lexer->getPos();
    debug("not_expression");
    if (accept(TokenType::OpLNot))
    {
        node::NodePtr expr;
        if ((expr = not_expression()) == nullptr) return nullptr;
        return std::make_shared<node::UnaryOp>(TokenType::OpLNot, expr, 0, pos);
    }
    return assignment_expression();
}

/*
 *  not_expression
 */

node::NodePtr Parser::expression()
{
    node::NodePtr node;
    debug("expression");
    if ((node = not_expression()) == nullptr) return nullptr;
    return node;
}

/*
 * 'type' id decl_expression* end
 */

node::NodePtr Parser::struct_statement()
{
    debug("struct_statement");
    ctx = "type statement";
    auto pos = lexer->getPos();

    // 'type'
    if (!accept(TokenType::Struct)) return nullptr;

    // id
    expect(TokenType::Id, "missing type name");

    auto name = std::get<std::string>(lexer->getToken().value);
    auto strukt = std::make_shared<node::Struct>(name, pos);

    lexer->getNextToken();

    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    // type fields
    do
    {
        node::NodePtr decl = expect(decl_expression(true), "expecting field");

        // semicolon might have been inserted here
        accept(TokenType::Semicolon);

        strukt->fields.emplace_back(decl->as<node::Decl>());
    } while (!accept(TokenType::End));

    return strukt;
}

/*
 * 'def' id '(' args? ')' (':' type_expression)? block
 */

node::NodePtr Parser::function_statement()
{
    node::BlockPtr body;
    debug("function_statement");
    ctx = "function statement";

    auto fnProto = expect(
            function_proto(NodeType::Function, TokenType::Def),
            "expected \"extern\" statement"
    )->as<node::Proto>();

    // block
    if ((body = block()) == nullptr)
        return nullptr;
    return std::make_shared<node::Function>(*fnProto.get(), body);
}

/*
 *  ('if' | 'unless') expression block
 *  ('else' 'if' block)*
 *  ('else' block)?
 */

node::NodePtr Parser::if_statement()
{
    node::NodePtr cond;
    node::BlockPtr body;
    auto pos = lexer->getPos();
    debug("if_statement");

    // ('if' | 'unless')
    if (!(lexer->isType(TokenType::If) || lexer->isType(TokenType::Unless))) return nullptr;
    int negate = lexer->isType(TokenType::Unless);

    lexer->getNextToken();

    // expression
    ctx = "if statement condition";
    if ((cond = expression()) == nullptr) return nullptr;

    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    // block
    ctx = "if statement";
    if ((body = block()) == nullptr)
    {
        return nullptr;
    }

    auto node = std::make_shared<node::If>(negate, cond, body, pos);

    // 'else'
    loop:
    {
        if (accept(TokenType::Else))
        {
            node::BlockPtr bodyNode;

            // ('else' 'if' block)*
            if (accept(TokenType::If))
            {
                auto linePos = lexer->getPos();
                ctx = "else if statement condition";
                if ((cond = expression()) == nullptr) return nullptr;

                // semicolon might have been inserted here
                accept(TokenType::Semicolon);

                ctx = "else if statement";
                if ((bodyNode = block()) == nullptr) return nullptr;
                node->elseIfs.emplace_back(std::make_shared<node::If>(0, cond, bodyNode, linePos));
                goto loop;
                // 'else'
            }
            else
            {
                ctx = "else statement";
                if ((bodyNode = block()) == nullptr) return nullptr;
                node->elseBlock = bodyNode;
            }
        }
    }
    return node;
}

/*
 * ('while' | 'until') expression block
 */

node::NodePtr Parser::while_statement()
{
    node::NodePtr cond;
    node::BlockPtr body;
    auto pos = lexer->getPos();
    debug("while_statement");

    // ('until' | 'while')
    if (!(lexer->isType(TokenType::Until) || lexer->isType(TokenType::While))) return nullptr;
    int negate = lexer->isType(TokenType::Until);
    ctx = "while statement condition";

    lexer->getNextToken();

    // expression
    if ((cond = expression()) == nullptr) return nullptr;
    ctx = "while statement";

    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    // block
    if ((body = block()) == nullptr) return nullptr;

    return std::make_shared<node::While>(negate, cond, body, pos);
}

/*
 * 'repeat' block ("'while' | 'unless') expression ";"
 */


node::NodePtr Parser::repeat_statement()
{
    node::NodePtr cond;
    node::BlockPtr body;
    auto pos = lexer->getPos();
    ctx = "while statement";
    debug("repeat_statement");
    // 'repeat'
    if (!accept(TokenType::Repeat)) return nullptr;

    ctx = "while statement body";
    // block
    if ((body = block()) == nullptr) return nullptr;

    // ('until' | 'while')
    if (!(lexer->isType(TokenType::Until) || lexer->isType(TokenType::While))) return nullptr;
    int negate = lexer->isType(TokenType::Until);
    lexer->getNextToken();
    ctx = "while statement condition";
    // expression
    if ((cond = expression()) == nullptr) return nullptr;
    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    return std::make_shared<node::Repeat>(negate, cond, body, pos);
}


/*
 * 'for' variable_expression ; expr ; expression ; block
 */

node::NodePtr Parser::for_statement()
{
    auto pos = lexer->getPos();
    ctx = "for statement";
    debug("for_statement");
    if (!accept(TokenType::For)) return nullptr;

    node::NodePtr beg = nullptr;
    node::NodePtr cond = nullptr;
    node::NodePtr step = nullptr;
    node::BlockPtr body = nullptr;

    // variable_expression
    ctx = "for statement begin";
    if ((beg = expression()) == nullptr) return nullptr;
    expectNext(TokenType::Semicolon, "expecting semicolon");

    // expression
    ctx = "for statement condition";
    if ((cond = expression()) == nullptr) return nullptr;
    expectNext(TokenType::Semicolon, "expecting semicolon");

    // expression
    ctx = "for statement step";
    if ((step = expression()) == nullptr) return nullptr;
    expectNext(TokenType::Semicolon, "expecting semicolon");

    // block
    ctx = "for statement block";
    if ((body = block()) == nullptr) return nullptr;

    return std::make_shared<node::For>(beg, cond, step, body, pos);
}

/*
 *   'return' expression
 * | 'return'
 */

node::NodePtr Parser::return_statement()
{
    auto pos = lexer->getPos();
    debug("return");
    ctx = "return statement";

    // 'return'
    if (!accept(TokenType::Return)) return nullptr;

    // 'return' expression
    node::NodePtr node = nullptr;

    if (!accept(TokenType::Semicolon))
    {
        if ((node = expression()) == nullptr) return nullptr;
    }
    return std::make_shared<node::Return>(node, pos);
}


/*
 *   'continue'
 */
node::NodePtr Parser::continue_statement()
{
    auto pos = lexer->getPos();
    debug("continue");
    ctx = "continue statement";

    // 'return'
    if (!accept(TokenType::Continue)) return nullptr;

    accept(TokenType::Semicolon); // "eat" semicolon

    return std::make_shared<node::Continue>(pos);
}


/*
 *   'break'
 */
node::NodePtr Parser::break_statement()
{
    auto pos = lexer->getPos();
    debug("break");
    ctx = "break statement";

    // 'return'
    if (!accept(TokenType::Break)) return nullptr;

    accept(TokenType::Semicolon); // "eat" semicolon

    return std::make_shared<node::Break>(pos);
}

/*
 * 'use' string ('as' id)?
 */

node::NodePtr Parser::use_statement()
{
    //puts("asçkdhakjshd");

    auto pos = lexer->getPos();
    debug("use");
    ctx = "use statement";

    // 'use'
    if (!accept(TokenType::Use)) return nullptr;
    auto node = std::make_shared<node::Use>(pos);

    // string
    expect(TokenType::String, "missing module name");

    node->module = std::get<std::string>(lexer->getToken().value);

    lexer->getNextToken();

    // 'as'
    if (accept(TokenType::As))
    {
        // id
        expect(TokenType::Id, "missing alias name");

        node->alias = std::get<std::string>(lexer->getToken().value);
        lexer->getNextToken();
    }

    return node;
}

node::NodePtr Parser::function_proto(NodeType ntype, TokenType ttype)
{
    auto pos = lexer->getPos();
    ctx = "proto statement";
    std::vector<node::NodePtr> params;
    node::TypePtr rtype = nullptr;
    bool isPublic;
    std::string name;
    std::vector<std::string> nsArr;

    // 'pub'
    isPublic = accept(TokenType::Pub);

    if (!accept(ttype)) return nullptr;

    // id

    do
    {
        expect(TokenType::Id, "missing function name");
        nsArr.push_back(std::get<std::string>(lexer->getToken().value));
        lexer->getNextToken();
    } while (accept(TokenType::OpModScope));
    name = nsArr.back();
    nsArr.pop_back();

    // '('
    if (accept(TokenType::LParen))
    {
        // params?
        params = function_params();
        // ')'
        ctx = "proto statement";
        expectNext(TokenType::RParen, "missing closing ')'");
    }

    ctx = "proto statement";

    // (':' type_expression)?
    if (accept(TokenType::Colon))
        rtype = expect(type_expression(), "missing type after ':'")->as<node::Type>();
    else
        rtype = std::make_shared<node::Type>("void", false, lexer->getPos()); // default void

    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    return std::make_shared<node::Proto>(ntype, isPublic, nsArr, name, rtype, params, pos);
}

/*
 * 'extern' id '(' args? ')' (':' type_expression)?
 */

node::NodePtr Parser::extern_statement()
{
    debug("extern");
    ctx = "extern statement";

    auto proto = expect(
            function_proto(NodeType::Extern, TokenType::Extern),
            "expected \"extern\" statement"
    );

    return proto->as<node::Extern>();
}

/*
 *  'case' expression block
 *  ('when' expr block)*
 *  ('else' block)?
 */

node::NodePtr Parser::case_statement()
{
    auto casePos = lexer->getPos();
    std::vector<node::WhenPtr> whenStmts;
    node::NodePtr caseExpr = nullptr;
    node::BlockPtr elseBlock = nullptr;
    auto node = std::make_shared<node::Case>(lexer->getPos());

    debug("case_statement");
    ctx = "case statement";

    if (!accept(TokenType::Case)) return nullptr;

    caseExpr = expect(expression(), "expected expression");
    accept(TokenType::Semicolon);

    do
    {
        if (lexer->isType(TokenType::When))
        {
            auto whenPos = lexer->getPos();
            ctx = "when statement";
            lexer->getNextToken();
            std::vector<node::NodePtr> whenExprs;
            do
            {
                whenExprs.push_back(expect(expression(), "expected expression"));
            } while (accept(TokenType::Comma));

            accept(TokenType::Semicolon);

            auto whenBody = expect(block(), "expected block")->as<node::Block>();
            auto when = std::make_shared<node::When>(whenExprs, whenBody, whenPos);
            whenStmts.push_back(when);
        }
        else if (lexer->isType(TokenType::Else))
        {
            ctx = "else statement";
            lexer->getNextToken();
            elseBlock = expect(block(), "expected else block")->as<node::Block>();
            break;
        }
    } while (!accept(TokenType::End));

    ctx = "case statement";

    return std::make_shared<node::Case>(caseExpr, whenStmts, elseBlock, casePos);
}

node::NodePtr Parser::mod_statement()
{
    debug("mod_statement");
    ctx = "module statement";
    auto pos = lexer->getPos();
    std::string name;
    std::vector<node::NodePtr> stmts;

    // 'type'
    if (!accept(TokenType::Mod)) return nullptr;

    // id
    expect(TokenType::Id, "missing type name");

    name = std::get<std::string>(lexer->getToken().value);
    lexer->getNextToken();
    // semicolon might have been inserted here
    accept(TokenType::Semicolon);

    do
    {
        stmts.emplace_back(expect(pub_modifier(), "expecting field"));
        accept(TokenType::Semicolon);
    } while (!accept(TokenType::End));

    return std::make_shared<node::Module>(name, stmts, pos);
}

node::NodePtr Parser::pub_modifier()
{
    bool isPublic = accept(TokenType::Pub);
    node::NodePtr stmt = statement();
    if (stmt == nullptr) return nullptr;
    if (stmt->is(NodeType::Extern) || stmt->is(NodeType::Function))
    {
        stmt->as<node::Proto>()->isPublic = isPublic;
    }
    return stmt;
}

/*
 *   if_statement
 * | while_statement
 * | return_statement
 * | function_statement
 * | struct_statement
 * | use_statement
 * | extern_statement
 * | repeat_statement
 * | for_statement
 * | case_statement
 * | mod_statement
 * | pub_modifier
 * | expression
 */

node::NodePtr Parser::statement()
{
    debug("statement");
    ctx = "statement";
    if (lexer->isType(TokenType::If) || lexer->isType(TokenType::Unless)) return if_statement();
    if (lexer->isType(TokenType::While) || lexer->isType(TokenType::Until)) return while_statement();
    if (lexer->isType(TokenType::Return)) return return_statement();
    if (lexer->isType(TokenType::Continue)) return continue_statement();
    if (lexer->isType(TokenType::Break)) return break_statement();
    if (lexer->isType(TokenType::Def)) return function_statement();
    if (lexer->isType(TokenType::Struct)) return struct_statement();
    if (lexer->isType(TokenType::Use)) return use_statement();
    if (lexer->isType(TokenType::Extern)) return extern_statement();
    if (lexer->isType(TokenType::Repeat)) return repeat_statement();
    if (lexer->isType(TokenType::For)) return for_statement();
    if (lexer->isType(TokenType::Case)) return case_statement();
    if (lexer->isType(TokenType::Mod)) return mod_statement();
    if (lexer->isType(TokenType::Pub)) return pub_modifier();
    return expression();
}

/*
 * statement* 'end'
 */

node::BlockPtr Parser::block()
{
    debug("block");
    node::NodePtr node;
    auto block = std::make_shared<node::Block>(lexer->getPos());

    if (accept(TokenType::End)) return block;

    do
    {
        if ((node = statement()) == nullptr)
            return nullptr;

        accept(TokenType::Semicolon);

        block->stmts.emplace_back(node);
    } while (!accept(TokenType::End)
             && !lexer->isType(TokenType::Else)
             && !lexer->isType(TokenType::While)
             && !lexer->isType(TokenType::Until)
             && !lexer->isType(TokenType::When));

    return block;
}

/*
 * Parse input.
 */

node::BlockPtr Parser::parse()
{
    debug("program");
    auto block = std::make_shared<node::Block>(lexer->getPos());


    lexer->getNextToken();
    while (!lexer->isType(TokenType::EOS))
    {
        node::NodePtr node;
        if ((node = statement()) == nullptr)
            throw ParseException(this);

        accept(TokenType::Semicolon);
        block->stmts.emplace_back(node);
    }

    return block;
}

node::IdPtr Parser::getId()
{
    expect(TokenType::Id, "expecting identifier");
    auto id = std::make_shared<node::Id>(std::get<std::string>(lexer->getToken().value).c_str(), lexer->getPos());
    lexer->getNextToken();
    return id;
}
