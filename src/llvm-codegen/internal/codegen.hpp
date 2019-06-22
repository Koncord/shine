#pragma once

#include <memory>
#include <ast/visitor.hpp>
#include <types.hpp>
#include <llvm-codegen/codegen.hpp>
#include <llvm/IR/IRBuilder.h>
#include <stack>
#include <utils/stack.hpp>

namespace shine
{
    struct LLVMCtx
    {
        std::unique_ptr<llvm::LLVMContext> ctx;
        std::unique_ptr<llvm::Module> module;
    };

    struct LLVMValue
    {
        LLVMValue() = default;
        LLVMValue(llvm::Value *value): value(value) {}
        LLVMValue(llvm::Value *value, bool sign): value(value), sign(sign) {}
        LLVMValue(llvm::Value *value, bool sign, bool constant): value(value), sign(sign), constant(constant) {}

        LLVMValue(llvm::Type *type): isType(true), type(type) {}

        union
        {
            llvm::Type *type;
            llvm::Value *value = nullptr;
        };

        bool isType = false;
        bool sign = false;
        bool constant = false;
        inline bool isGlobal() { return llvm::isa<llvm::GlobalValue>(value); };
        //inline bool isConstant() { return llvm::isa<llvm::Constant>(value); };
    };

    class NamedVariables
    {
    public:
        typedef std::unordered_map<std::string, LLVMValue> NamedVariablesT;
    private:
        util::stack<NamedVariablesT> namedVariablesStack;
        int varLevel;
    public:
        NamedVariables(): varLevel(-1) {}
        LLVMValue *findNamedVariable(const std::string &name);
        void pushLevel();
        void popLevel();
        void insertVariable(std::string, LLVMValue val);
        NamedVariablesT &top(); // todo: get rid
        const NamedVariablesT &top() const;
    };

    class LLVMCodegenImpl : public Visitor
    {
    public:
        LLVMCodegenImpl(const node::NodePtr &root, LLVMCtx *llvmctx, std::string filename);

    private:
        LLVMCtx *llvmctx;
        std::unique_ptr<llvm::IRBuilder<>> builder;


        struct Func
        {
            LLVMValue value;
            std::vector<bool> argSigns;
        };
        std::unordered_map<std::string, Func> functions;
        std::map<std::string, llvm::Type*> registeredTypes;
        std::map<std::string, std::map<std::string, int>> structTypeIdx;
        util::stack<LLVMValue> valStack;
        NamedVariables namedVariables;
        util::stack<llvm::BasicBlock*> breakStack;
        util::stack<llvm::BasicBlock*> loopContinueStack;
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

        llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *function, llvm::Type *type, const std::string &varName);

        std::string getName(const node::NodePtr &node);

        LLVMValue popValue();
        void pushValue(LLVMValue value);

        bool isIntegerType(std::string ref, int &bitwidth, bool *isSigned = nullptr);

        struct Params
        {
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

        llvm::Value *createCondition(const node::NodePtr &node, const node::NodePtr &expr, const std::string &conditionName = "");

        uint64_t sizeOf(const node::NodePtr &node);
        void registerBuiltinNumericTypes();
        void registerType(const std::string &typeName, llvm::Type *type);
        llvm::Type *getRegisteredType(const std::string &typeName, bool getPtrType);

        static bool isBlockContains(const node::BlockPtr &block, NodeType nodeType);
        static bool isBlockContainsJumpCond(const node::BlockPtr &block);
    public:

        void visit_block(const node::BlockPtr &node) override;
        void visit_id(const node::IdPtr &node) override;
        void visit_int(const node::IntPtr &node) override;
        void visit_float(const node::FloatPtr &node) override;
        void visit_string(const node::StringPtr &node) override;
        void visit_boolean(const node::BooleanPtr &node) override;
        void visit_slot(const node::SlotPtr &node) override;
        void visit_call(const node::CallPtr &node) override;
        void visit_unary_op(const node::UnaryOpPtr &node) override;
        void visit_binary_op(const node::BinaryOpPtr &node) override;
        void visit_function(const node::FunctionPtr &node) override;
        void visit_array(const node::ArrayPtr &node) override;
        void visit_hash(const node::HashPtr &node) override;
        void visit_return(const node::ReturnPtr &node) override;
        void visit_decl(const node::DeclPtr &node) override;
        void visit_while(const node::WhilePtr &node) override;
        void visit_repeat(const node::RepeatPtr &node) override;
        void visit_for(const node::ForPtr &node) override;
        void visit_if(const node::IfPtr &node) override;
        void visit_subscript(const node::SubscriptPtr &node) override;
        void visit_type(const node::TypePtr &node) override;
        void visit_let(const node::LetPtr &node) override;
        void visit_const(const node::ConstPtr &node) override;
        void visit_use(const node::UsePtr &node) override;
        void visit_extern(const node::ExternPtr &node) override;
        void visit_continue(const node::ContinuePtr &node) override;
        void visit_break(const node::BreakPtr &node) override;
        void visit_struct(const node::StructPtr &node) override;
        void visit_vaarg(const node::VaArgPtr &node) override;
        void visit_case(const node::CasePtr &node) override;
    };
}

