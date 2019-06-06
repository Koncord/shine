#pragma once

#include <memory>
#include "ast.hpp"

namespace shine
{
    class Visitor
    {
    public:
        std::string filename;
        void visit(const node::NodePtr &node);

    protected:
        virtual void visit_block(const node::BlockPtr &node) = 0;
        virtual void visit_id(const node::IdPtr &node) = 0;
        virtual void visit_int(const node::IntPtr &node) = 0;
        virtual void visit_float(const node::FloatPtr &node) = 0;
        virtual void visit_string(const node::StringPtr &node) = 0;
        virtual void visit_boolean(const node::BooleanPtr &node) = 0;
        virtual void visit_slot(const node::SlotPtr &node) = 0;
        virtual void visit_call(const node::CallPtr &node) = 0;
        virtual void visit_while(const node::WhilePtr &node) = 0;
        virtual void visit_repeat(const node::RepeatPtr &node) = 0;
        virtual void visit_for(const node::ForPtr &node) = 0;
        virtual void visit_unary_op(const node::UnaryOpPtr &node) = 0;
        virtual void visit_binary_op(const node::BinaryOpPtr &node) = 0;
        virtual void visit_function(const node::FunctionPtr &node) = 0;
        virtual void visit_array(const node::ArrayPtr &node) = 0;
        virtual void visit_hash(const node::HashPtr &node) = 0;
        virtual void visit_return(const node::ReturnPtr &node) = 0;
        virtual void visit_decl(const node::DeclPtr &node) = 0;
        virtual void visit_if(const node::IfPtr &node) = 0;
        virtual void visit_subscript(const node::SubscriptPtr &node) = 0;
        virtual void visit_type(const node::TypePtr &node) = 0;
        virtual void visit_let(const node::LetPtr &node) = 0;
        virtual void visit_const(const node::ConstPtr &node) = 0;
        virtual void visit_use(const node::UsePtr &node) = 0;
        virtual void visit_extern(const node::ExternPtr &node) = 0;
        virtual void visit_continue(const node::ContinuePtr &node) = 0;
        virtual void visit_break(const node::BreakPtr &node) = 0;
        virtual void visit_struct(const node::StructPtr &node) = 0;
        virtual void visit_vaarg(const node::VaArgPtr &node) = 0;
        virtual void visit_case(const node::CasePtr &node) = 0;
    };
}