#pragma once

#include <string>
#include <ast/ast.hpp>

namespace shine
{
    class ParseException;
    class Parser
    {
        friend class ParseException;
    private:
        node::NodePtr expression();
        node::NodePtr not_exprssion();
        node::NodePtr assignment_exprssion();
        node::NodePtr paren_exprssion();
        node::NodePtr array_exprssion();
        node::NodePtr hash_exprssion();
        node::NodePtr type_exprssion();
        node::NodePtr decl_exprssion(bool needType);
        node::NodePtr call_exprssion(node::NodePtr left);
        node::NodePtr primary_exprssion();
        node::NodePtr unary_exprssion();
        node::NodePtr multiplicative_exprssion();
        node::NodePtr postfix_exprssion();
        node::NodePtr additive_exprssion();
        node::NodePtr shift_exprssion();
        node::NodePtr relational_exprssion();
        node::NodePtr equality_exprssion();
        node::NodePtr bitwise_and_exprssion();
        node::NodePtr bitwise_xor_exprssion();
        node::NodePtr bitswise_or_exprssion();
        node::NodePtr logical_or_exprssion();
        node::NodePtr logical_and_exprssion();
        node::NodePtr slot_access_exprssion(node::NodePtr left);
        node::NodePtr variable_exprssion(const TokenType &type);

        node::NodePtr statement();
        node::NodePtr struct_statement();
        node::NodePtr extern_statement();
        node::NodePtr function_statement();
        node::NodePtr if_statement();
        node::NodePtr case_statement();
        node::NodePtr while_statement();
        node::NodePtr repeat_statement();
        node::NodePtr for_statement();
        node::NodePtr return_statement();
        node::NodePtr use_statement();
        node::NodePtr continue_statement();
        node::NodePtr break_statement();

        node::BlockPtr block();

        bool arg_list(std::vector<node::NodePtr> &vals, TokenType delim);
        node::ArgsPtr call_args();

        bool hash_pairs(std::vector<node::HashPairPtr> &pairs, TokenType delim);
        std::vector<node::NodePtr> function_params();

    public:
        Parser(Lexer *lex) : lexer(lex) {}

        node::BlockPtr parse();

        Lexer *getLexer() const { return lexer; }

    private:
        Lexer *lexer;
        std::string ctx;

        void debug(std::string_view str);
        bool accept(TokenType t);
        void expect(TokenType t, const std::string &error);
        void expectNext(TokenType t, const std::string &error);
        const node::NodePtr &expect(const node::NodePtr &node, const std::string &error);
    };
}
