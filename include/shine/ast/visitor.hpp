#pragma once

#include <memory>
#include "astTypes.hpp"

namespace shine {
    class Visitor {
    public:
        virtual void visit(const node::BlockPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::IdPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::IntPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::FloatPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::StringPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::BooleanPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::SlotPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::CallPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::WhilePtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::RepeatPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ForPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::UnaryOpPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::BinaryOpPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::FunctionPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ArrayPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::HashPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ReturnPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::DeclPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::IfPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::SubscriptPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::TypePtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::LetPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ConstPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::UsePtr &node, const node::NodePtr &invoker) = 0;
        //virtual void visit(const node::ExternPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ContinuePtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::BreakPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::StructPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::VaArgPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::CasePtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ModulePtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ScopePtr &node, const node::NodePtr &invoker) = 0;
        //virtual void visit(const node::ArgsPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::ProtoPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::WhenPtr &node, const node::NodePtr &invoker) = 0;
        virtual void visit(const node::HashPairPtr &node, const node::NodePtr &invoker) = 0;
    };
}
