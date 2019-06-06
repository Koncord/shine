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
            case NodeType::Block:
                visit_block(node->as<node::Block>());
                break;
            case NodeType::Id:
                visit_id(node->as<node::Id>());
                break;
            case NodeType::Decl:
                visit_decl(node->as<node::Decl>());
                break;
            case NodeType::Let:
                visit_let(node->as<node::Let>());
                break;
            case NodeType::Const:
                visit_const(node->as<node::Const>());
                break;
            case NodeType::Int:
                visit_int(node->as<node::Int>());
                break;
            case NodeType::Float:
                visit_float(node->as<node::Float>());
                break;
            case NodeType::String:
                visit_string(node->as<node::String>());
                break;
            case NodeType::Boolean:
                visit_boolean(node->as<node::Boolean>());
                break;
            case NodeType::Subscript:
                visit_subscript(node->as<node::Subscript>());
                break;
            case NodeType::Slot:
                visit_slot(node->as<node::Slot>());
                break;
            case NodeType::Call:
                visit_call(node->as<node::Call>());
                break;
            case NodeType::If:
                visit_if(node->as<node::If>());
                break;
            case NodeType::While:
                visit_while(node->as<node::While>());
                break;
            case NodeType::Repeat:
                visit_repeat(node->as<node::Repeat>());
                break;
            case NodeType::UnaryOp:
                visit_unary_op(node->as<node::UnaryOp>());
                break;
            case NodeType::BinaryOp:
                visit_binary_op(node->as<node::BinaryOp>());
                break;
            case NodeType::Function:
                visit_function(node->as<node::Function>());
                break;
            case NodeType::Type:
                visit_type(node->as<node::Type>());
                break;
            case NodeType::Array:
                visit_array(node->as<node::Array>());
                break;
            case NodeType::Hash:
                visit_hash(node->as<node::Hash>());
                break;
            case NodeType::Return:
                visit_return(node->as<node::Return>());
                break;
            case NodeType::Use:
                visit_use(node->as<node::Use>());
                break;
            case NodeType::Extern:
                visit_extern(node->as<node::Extern>());
                break;
            case NodeType::Continue:
                visit_continue(node->as<node::Continue>());
                break;
            case NodeType::Break:
                visit_break(node->as<node::Break>());
                break;
            case NodeType::Struct:
                visit_struct(node->as<node::Struct>());
                break;
            case NodeType::VaArg:
                visit_vaarg(node->as<node::VaArg>());
                break;
            case NodeType::For:
                visit_for(node->as<node::For>());
                break;
            case NodeType::Case:
                visit_case(node->as<node::Case>());
                break;

            default:
                throw UnhandledNode(filename, node);
        }
    }

}
