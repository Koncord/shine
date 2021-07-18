#pragma once

#include <memory>

namespace shine {
    namespace node {
        struct Node;
        typedef std::shared_ptr<node::Node> NodePtr;
    }
    struct Lexer;
    struct Parser;
}
