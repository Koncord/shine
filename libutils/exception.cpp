#include <utils/exception.hpp>

#include <sstream>

namespace shine {
    ShineException::ShineException(std::string filename, Position pos, const std::string &msg) {
        std::stringstream sstr;
        sstr << filename << ':' << pos.lineno << ':' << pos.linepos << ": " << msg;
        this->msg = sstr.str();
    }

    const char *ShineException::what() const noexcept {
        return msg.c_str();
    }
}
