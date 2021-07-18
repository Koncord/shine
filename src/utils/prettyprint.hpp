#pragma once

#include <types.hpp>

namespace shine {
    // default printf
    void SetPrettyPrintFunc(int (*func)(const char *format, ...));

    void PrettyPrint(const shine::node::NodePtr &node);
}
