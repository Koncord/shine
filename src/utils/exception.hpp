#pragma once

#include <exception>
#include <string>
#include <lexer/position.hpp>
#include "types.hpp"

namespace shine
{
    class ShineException: public std::exception
    {
    protected:
        std::string msg;
    public:
        ShineException(std::string filename, Position pos, const std::string& msg);
        const char* what() const noexcept final;
    };

    class ParseException: public ShineException
    {
    protected:
        Lexer *lexer = nullptr;
        Parser *parser = nullptr;
    public:
        explicit ParseException(Lexer *lexer, const std::string& msg = "");
        explicit ParseException(Parser *parser, const std::string& msg = "");
        ~ParseException() override = default;
    };

    class UnhandledNode : public ShineException
    {
    public:
        UnhandledNode(const std::string &filename, const node::NodePtr& node);
    };
}

