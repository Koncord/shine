#include "exception.hpp"

#include <lexer/lexer.hpp>
#include <ast/parser.hpp>
#include <sstream>
#include <cstring>
#include <utility>

namespace shine
{
    ShineException::ShineException(std::string filename, Position pos, const std::string &msg)
    {
        std::stringstream sstr;
        sstr << filename << ':' << pos.lineno << ':' << pos.linepos << ": " << msg;
        this->msg = sstr.str();
    }

    const char *ShineException::what() const noexcept
    {
        return msg.c_str();
    }

    ParseException::ParseException(Lexer *lexer, const std::string &msg)
            : lexer(lexer),
              ShineException(lexer->filename, lexer->getPos(), "Syntax error:")
    {
        std::stringstream sstr;
        sstr << " " << msg << ".\n";

        this->msg += sstr.str();
    }

    ParseException::ParseException(Parser *parser, const std::string &msg)
            : ParseException(parser, parser->lexer->getPos(), msg) {}

    ParseException::ParseException(Parser *parser, Position pos, const std::string &msg)
            : lexer(parser->lexer),
              ShineException(parser->lexer->filename, pos, "Parse error in ")
    {
        std::stringstream sstr;

        if (msg.empty())
            sstr << "unexpected token '" << std::string(TokenHelper::getTokenTypeString(lexer->tok.type)) << '\'';
        else
            sstr << msg;

        sstr << ".\n";

        this->msg += parser->ctx + ": ";
        this->msg += sstr.str();
    }

    UnhandledNode::UnhandledNode(const std::string &filename, const node::NodePtr &node)
            : ShineException(filename, node->pos, "Unhandled node type:")
    {
        std::stringstream sstr;
        sstr << ' ' << node->getNodeName();
        if (node->is(NodeType::Id))
            sstr << ", node value: " << node->as<node::Id>()->val;

        this->msg += sstr.str();
    }
}
