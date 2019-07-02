#include <utils/exception.hpp>
#include "visitor.hpp"

namespace shine
{
    void Visitor::visit(const node::NodePtr &node)
    {
        if (node == nullptr)
            return;
        switch (node->nodeType)
        {
#define CASE(n1, n2) case NodeType::n1: visit_##n2(node->as<node::n1>()); break;
            CASE(Block, block)
            CASE(Id, id)
            CASE(Decl, decl)
            CASE(Let, let)
            CASE(Const, const)
            CASE(Int, int)
            CASE(Float, float)
            CASE(String, string)
            CASE(Boolean, boolean)
            CASE(Subscript, subscript)
            CASE(Slot, slot)
            CASE(Call, call)
            CASE(If, if)
            CASE(While, while)
            CASE(Repeat, repeat)
            CASE(UnaryOp, unary_op)
            CASE(BinaryOp, binary_op)
            CASE(Function, function)
            CASE(Type, type)
            CASE(Array, array)
            CASE(Hash, hash)
            CASE(Return, return)
            CASE(Use, use)
            CASE(Extern, extern)
            CASE(Continue, continue)
            CASE(Break, break)
            CASE(Struct, struct)
            CASE(VaArg, vaarg)
            CASE(For, for)
            CASE(Case, case)
            CASE(Module, module)
            CASE(Scope, scope)
            default:
                throw UnhandledNode(filename, node);
#undef CASE
        }
    }

}
