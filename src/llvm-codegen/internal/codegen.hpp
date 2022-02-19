#pragma once

#include <memory>
#include <utility>
#include <ast/visitor.hpp>
#include <types.hpp>
#include <llvm-codegen/codegen.hpp>
#include <llvm/IR/IRBuilder.h>
#include <stack>
#include <utils/stack.hpp>
#include "typeInfo.hpp"

namespace shine {
#define SHINE_UNUSED(arg) (void)(arg);
    struct LLVMCtx {
        std::unique_ptr<llvm::LLVMContext> ctx;
        std::unique_ptr<llvm::Module> module;
    };

    struct LLVMValue {
        LLVMValue() = default;

        LLVMValue(llvm::Value *value) : value(value) {}

        LLVMValue(llvm::Value *value, bool sign) : value(value), sign(sign) {}

        LLVMValue(llvm::Value *value, bool sign, bool constant) : value(value), sign(sign), constant(constant) {}

        LLVMValue(llvm::Type *type, TypeInfo typeInfo) : type(type), isType(true), typeInfo(std::move(typeInfo)) {}

        union {
            llvm::Type *type;
            llvm::Value *value = nullptr;
        };

        bool isType = false;
        bool sign = false;
        bool constant = false;
        TypeInfo typeInfo;

        inline bool isGlobal() { return llvm::isa<llvm::GlobalValue>(value); };
        //inline bool isConstant() { return llvm::isa<llvm::Constant>(value); };
    };

    class NamedVariables {
    public:
        typedef std::unordered_map<std::string, LLVMValue> NamedVariablesT;
    private:
        util::stack<NamedVariablesT> namedVariablesStack;
        int varLevel;
    public:
        NamedVariables() : varLevel(-1) {}

        LLVMValue *findNamedVariable(const std::string &name);
        void pushLevel();
        void popLevel();
        void insertVariable(std::string, LLVMValue val);
        NamedVariablesT &top(); // todo: get rid
        const NamedVariablesT &top() const;
    };

    class LLVMCodegenImpl : public Visitor {
    public:
        LLVMCodegenImpl(const node::NodePtr &root, LLVMCtx *llvmctx, std::string filename);

    private:
        std::string filename;
        LLVMCtx *llvmctx;
        std::unique_ptr<llvm::IRBuilder<>> builder;


        struct Func {
            LLVMValue value;
            std::vector<bool> argSigns;
        };
        std::unordered_map<std::string, Func> functions;
        std::map<std::string, llvm::Type *> registeredTypes;
        std::map<std::string, std::map<std::string, int>> structTypeIdx;
        util::stack<LLVMValue> valStack;
        NamedVariables namedVariables;
        util::stack<llvm::BasicBlock *> breakStack;
        util::stack<llvm::BasicBlock *> loopContinueStack;
        node::NodePtr root;

        llvm::Value *IntCast(
                const LLVMValue &leftValue,
                const LLVMValue &rightValue
        );

        llvm::Value *FPCast(
                const LLVMValue &leftValue,
                const LLVMValue &rightValue
        );

        LLVMValue createBinaryOperation(
                const node::BinaryOpPtr &bin,
                const LLVMValue &leftValue,
                const LLVMValue &rightValue
        );

        LLVMValue createUnaryOperation(
                const node::UnaryOpPtr &uno,
                const LLVMValue &value
        );

        llvm::AllocaInst *
        CreateEntryBlockAlloca(llvm::Function *function, llvm::Type *type, const std::string &varName);

        std::string getName(const node::NodePtr &node);

        LLVMValue popValue();
        void pushValue(LLVMValue const &llvmValue);
        void pushValue(llvm::Type *type, TypeInfo typeInfo);

        static bool isIntegerType(std::string ref, int &bitwidth, bool *isSigned = nullptr);

        struct Params {
            std::vector<llvm::Type *> params;
            std::vector<bool> signs;
            std::vector<std::string> names;
            bool isVarArg = false;
        };

        Params getLLVMParams(const std::vector<node::NodePtr> &params);

        llvm::Function *createProto(
                const std::string &name,
                const node::TypePtr &nodeType,
                const std::vector<node::NodePtr> &params
        );

        llvm::Value *
        createCondition(const node::NodePtr &node, const node::NodePtr &expr, const std::string &conditionName = "");

        uint64_t sizeOf(const node::NodePtr &node);
        void registerBuiltinNumericTypes();
        void registerType(const std::string &typeName, llvm::Type *type);
        llvm::Type *getRegisteredType(const std::string &typeName, bool getPtrType);

        static bool isBlockContains(const node::BlockPtr &block, NodeType nodeType);
        static bool isBlockContainsJumpCond(const node::BlockPtr &block);
    public:

        void visit(const node::BlockPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::IdPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::IntPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::FloatPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::StringPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::BooleanPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::SlotPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::CallPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::UnaryOpPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::BinaryOpPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::FunctionPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ArrayPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::HashPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ReturnPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::DeclPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::WhilePtr &node, const node::NodePtr &invoker) override;
        void visit(const node::RepeatPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ForPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::IfPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::SubscriptPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::TypePtr &node, const node::NodePtr &invoker) override;
        void visit(const node::LetPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ConstPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::UsePtr &node, const node::NodePtr &invoker) override;
        //void visit(const node::ExternPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ContinuePtr &node, const node::NodePtr &invoker) override;
        void visit(const node::BreakPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::StructPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::VaArgPtr &node, const node::NodePtr &invoker) override;
        void visit(const node::CasePtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ModulePtr &node, const node::NodePtr &invoker) override;
        void visit(const node::ScopePtr &node, const node::NodePtr &invoker) override;
        //void visit(node::ArgsPtr const &node, const node::NodePtr &invoker) override;
        void visit(node::ProtoPtr const &node, const node::NodePtr &invoker) override;
        void visit(node::WhenPtr const &node, const node::NodePtr &invoker) override;
        void visit(node::HashPairPtr const &node, const node::NodePtr &invoker) override;
    };
}

