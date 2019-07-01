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
        node::NodePtr not_expression();
        node::NodePtr assignment_expression();
        node::NodePtr paren_expression();
        node::NodePtr array_expression();
        node::NodePtr hash_expression();
        node::NodePtr type_expression();
        node::NodePtr decl_expression(bool needType);
        node::NodePtr call_expression(node::NodePtr left);
        node::NodePtr primary_expression();
        node::NodePtr unary_expression();
        node::NodePtr multiplicative_expression();
        node::NodePtr postfix_expression();
        node::NodePtr additive_expression();
        node::NodePtr shift_expression();
        node::NodePtr relational_expression();
        node::NodePtr equality_expression();
        node::NodePtr bitwise_and_expression();
        node::NodePtr bitwise_xor_expression();
        node::NodePtr bitswise_or_expression();
        node::NodePtr logical_or_expression();
        node::NodePtr logical_and_expression();
        node::NodePtr slot_access_expression(node::NodePtr left);
        node::NodePtr variable_expression(const TokenType &type);

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
        node::NodePtr mod_statement();

        node::NodePtr pub_modifier();

        node::BlockPtr block();

        bool arg_list(std::vector<node::NodePtr> &vals, TokenType delim);
        node::ArgsPtr call_args();

        bool hash_pairs(std::vector<node::HashPairPtr> &pairs, TokenType delim);
        node::NodePtr function_proto(NodeType ntype, TokenType ttype);
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
