//
// Created by Stanislav "Koncord" Zhukov on 10.07.2021.
//

#include <utils/exception.hpp>
#include <lexer/lexer.hpp>
#include <ast/parser.hpp>
#include <utils/exception.hpp>
#include <sstream>

namespace shine {

    ParseException::ParseException(Parser *parser, const std::string &msg)
            : ParseException(parser, parser->lexer->getPosition(), msg) {}

    ParseException::ParseException(Parser *parser, Position pos, const std::string &msg)
            : ShineException(parser->lexer->filename, pos, "Parse error in "), lexer(parser->lexer) {
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
            : ShineException(filename, node->pos, "Unhandled node type:") {
        std::stringstream sstr;
        sstr << ' ' << node->getNodeName();
        if (node->is(NodeType::Id))
            sstr << ", node value: " << node->as<node::Id>()->val;

        this->msg += sstr.str();
    }

    AstException::AstException(node::NodePtr const &node)
            : ShineException("", node->pos, "") {
        std::stringstream sstr;
        sstr << "Node " << node->getNodeName() << ' ' << "must not be called via accept(Visitor &)";

        this->msg =  sstr.str();
    }
}
