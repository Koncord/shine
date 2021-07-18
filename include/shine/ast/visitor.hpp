#pragma once

#include <memory>
#include "astTypes.hpp"

namespace shine {
    class Visitor {
    public:
        virtual void visit(const node::BlockPtr &node) = 0;
        virtual void visit(const node::IdPtr &node) = 0;
        virtual void visit(const node::IntPtr &node) = 0;
        virtual void visit(const node::FloatPtr &node) = 0;
        virtual void visit(const node::StringPtr &node) = 0;
        virtual void visit(const node::BooleanPtr &node) = 0;
        virtual void visit(const node::SlotPtr &node) = 0;
        virtual void visit(const node::CallPtr &node) = 0;
        virtual void visit(const node::WhilePtr &node) = 0;
        virtual void visit(const node::RepeatPtr &node) = 0;
        virtual void visit(const node::ForPtr &node) = 0;
        virtual void visit(const node::UnaryOpPtr &node) = 0;
        virtual void visit(const node::BinaryOpPtr &node) = 0;
        virtual void visit(const node::FunctionPtr &node) = 0;
        virtual void visit(const node::ArrayPtr &node) = 0;
        virtual void visit(const node::HashPtr &node) = 0;
        virtual void visit(const node::ReturnPtr &node) = 0;
        virtual void visit(const node::DeclPtr &node) = 0;
        virtual void visit(const node::IfPtr &node) = 0;
        virtual void visit(const node::SubscriptPtr &node) = 0;
        virtual void visit(const node::TypePtr &node) = 0;
        virtual void visit(const node::LetPtr &node) = 0;
        virtual void visit(const node::ConstPtr &node) = 0;
        virtual void visit(const node::UsePtr &node) = 0;
        //virtual void visit(const node::ExternPtr &node) = 0;
        virtual void visit(const node::ContinuePtr &node) = 0;
        virtual void visit(const node::BreakPtr &node) = 0;
        virtual void visit(const node::StructPtr &node) = 0;
        virtual void visit(const node::VaArgPtr &node) = 0;
        virtual void visit(const node::CasePtr &node) = 0;
        virtual void visit(const node::ModulePtr &node) = 0;
        virtual void visit(const node::ScopePtr &node) = 0;
        //virtual void visit(const node::ArgsPtr &node) = 0;
        virtual void visit(const node::ProtoPtr &node) = 0;
        virtual void visit(const node::WhenPtr &node) = 0;
        virtual void visit(const node::HashPairPtr &node) = 0;
    };
}
