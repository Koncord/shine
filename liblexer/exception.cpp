//
// Created by Stanislav "Koncord" Zhukov on 10.07.2021.
//

#include <lexer/lexer.hpp>
#include <utils/exception.hpp>
#include <sstream>

namespace shine {
    LexerException::LexerException(Lexer *lexer, const std::string &msg)
            : ShineException(lexer->filename, lexer->getPosition(), "Syntax error:"), lexer(lexer) {
        std::stringstream sstr;
        sstr << " " << msg << ".\n";

        this->msg += sstr.str();
    }
}