#include <utils/exception.hpp>
#include <llvm/ADT/StringExtras.h>
#include "codegen.hpp"
#include <ast/ast.hpp>
#include <utility>

using namespace shine;
using namespace llvm;

LLVMCodegenImpl::LLVMCodegenImpl(const node::NodePtr &root, LLVMCtx *llvmctx, std::string filename) : llvmctx(llvmctx) {
    namedVariables.pushLevel();
    builder = std::make_unique<IRBuilder<>>(*llvmctx->ctx);

    this->filename = std::move(filename);
    this->root = root;

    registerBuiltinNumericTypes();
    createProto("alloca",
                std::make_shared<node::Type>("void", true, Position{}), {
                        std::make_shared<node::Decl>(std::vector<node::IdPtr> {
                                std::make_shared<node::Id>("sz", Position{})
                        }, std::make_shared<node::Type>("i64", true, Position{}), Position{})
                }
    );

    createProto("memcmp", std::make_shared<node::Type>("int", false, Position {}), {
            std::make_shared<node::Decl>(std::vector<node::IdPtr> {
                    std::make_shared<node::Id>("dst", Position{})
            }, std::make_shared<node::Type>("void", true, Position{}), Position{}),
            std::make_shared<node::Decl>(std::vector<node::IdPtr> {
                    std::make_shared<node::Id>("src", Position{})
            }, std::make_shared<node::Type>("void", true, Position{}), Position{}),
            std::make_shared<node::Decl>(std::vector<node::IdPtr> {
                    std::make_shared<node::Id>("n", Position{})
            }, std::make_shared<node::Type>("i64", false, Position{}), Position{})
    });

    root->accept(*this);
}

void LLVMCodegenImpl::registerType(const std::string &name, llvm::Type *type) {
    registeredTypes.emplace(name, type);
}

void LLVMCodegenImpl::registerBuiltinNumericTypes() {
    registerType("double", builder->getDoubleTy());
    registerType("float", builder->getFloatTy());
    registerType("boolean", builder->getInt1Ty());
    registerType("string", builder->getInt8PtrTy());

    for (uint32_t bitwidth = 8; bitwidth <= 64; bitwidth <<= 1) // integer types
    {
        std::string bw = std::to_string(bitwidth);
        registerType("u" + bw, builder->getIntNTy(bitwidth));
        registerType("i" + bw, builder->getIntNTy(bitwidth));
    }
}

Type *LLVMCodegenImpl::getRegisteredType(const std::string &typeName, bool /*getPtrType*/) {
    return registeredTypes.at(typeName);
}

AllocaInst *LLVMCodegenImpl::CreateEntryBlockAlloca(Function *function, Type *type, const std::string &varName) {
    IRBuilder<> TmpB(&function->getEntryBlock(), function->getEntryBlock().begin());
    return TmpB.CreateAlloca(type, nullptr, varName);
}

std::string LLVMCodegenImpl::getName(const node::NodePtr &node) {
    std::string name;
    if (node->is(NodeType::BinaryOp)) {
        const auto &binNode = node->as<node::BinaryOp>();
        if (binNode->left->is(NodeType::Id))
            name = binNode->left->as<node::Id>()->val;
        else
            name = binNode->left->as<node::Decl>()->vec.at(0)->as<node::Id>()->val;
    }
    return name;
}

LLVMValue LLVMCodegenImpl::popValue() {
    return valStack.pop();
}

void LLVMCodegenImpl::pushValue(LLVMValue const &llvmValue) {
    valStack.push(llvmValue);
}

void LLVMCodegenImpl::pushValue(llvm::Type *type, TypeInfo typeInfo) {
    valStack.push(LLVMValue(type, std::move(typeInfo)));
}

LLVMValue *NamedVariables::findNamedVariable(const std::string &name) // reverse search of variable
{
    int level = namedVariablesStack.size() - 1;
    for (auto it = namedVariablesStack.rbegin(); it != namedVariablesStack.rend(); ++it) {
        auto vname = name + "_" + std::to_string(level);
        if (auto v = std::find_if(it->begin(), it->end(), [&vname](const auto &val) { return val.first == vname; });
                v != it->end())
            return &v->second;
        level -= 1;
    }
    return nullptr;
}

void NamedVariables::pushLevel() {
    namedVariablesStack.push(NamedVariablesT());
    varLevel += 1;
}

void NamedVariables::popLevel() {
    namedVariablesStack.vpop();
    varLevel -= 1;
}

void NamedVariables::insertVariable(std::string name, LLVMValue val) {
    name += "_" + std::to_string(varLevel);
    top().emplace(name, val);
}

NamedVariables::NamedVariablesT &NamedVariables::top() {
    return namedVariablesStack.top();
}

const NamedVariables::NamedVariablesT &NamedVariables::top() const {
    return namedVariablesStack.top();
}

bool LLVMCodegenImpl::isIntegerType(std::string ref, int &bitwidth, bool *isSigned) {
    if (ref.size() < 2)
        return false;
    if (ref[0] == 'i' || ref[0] == 'u') {
        if (isSigned != nullptr)
            *isSigned = ref[0] == 'i';
        if (llvm::to_integer(StringRef(&ref[1]), bitwidth))
            return (bitwidth == 8 || bitwidth == 16 || bitwidth == 32 || bitwidth == 64);
    }
    return false;
}

LLVMCodegenImpl::Params LLVMCodegenImpl::getLLVMParams(const std::vector<node::NodePtr> &params) {
    LLVMCodegenImpl::Params args{};
    int ignored;
    for (const auto &arg : params) {
        if (arg->is(NodeType::VaArg)) {
            args.isVarArg = true;
            break;
        }
        if (!arg->is(NodeType::Decl))
            throw UnhandledNode(filename, arg);
        const auto &declNode = arg->as<node::Decl>();
        declNode->type->accept(*this, arg);
        Type *type = popValue().type;
        args.names.emplace_back(declNode->vec[0]->val);
        bool sign = false;
        isIntegerType(declNode->type->tname, ignored, &sign);
        args.params.emplace_back(type);
        args.signs.push_back(sign);
    }


    return args;
}

Function *LLVMCodegenImpl::createProto(
        const std::string &name,
        const node::TypePtr &nodeType,
        const std::vector<node::NodePtr> &params
) {
    int ignored;

    Function *llvmFunc;
    auto &func = functions[name];

    isIntegerType(nodeType->tname, ignored, &func.value.sign);

    auto args = getLLVMParams(params);

    func.argSigns = std::move(args.signs);

    nodeType->accept(*this);
    llvm::Type *retType = popValue().type;

    FunctionType *fnType = FunctionType::get(retType, args.params, args.isVarArg);
    auto linkage = Function::ExternalLinkage;
    llvmFunc = Function::Create(fnType, linkage, name, llvmctx->module.get());

    int idx = 0;
    for (auto &arg : llvmFunc->args())
        arg.setName(args.names[idx++]);

    func.value.value = llvmFunc;

    return llvmFunc;
}

Value *LLVMCodegenImpl::createCondition(
        const node::NodePtr &node,
        const node::NodePtr &expr,
        const std::string &conditionName
) {
    expr->accept(*this, node);
    LLVMValue condition = popValue();

    if (condition.value->getType()->isPointerTy())
        condition.value = builder->CreateLoad(condition.value->getType()->getPointerElementType(), condition.value);

    auto const &condTy = condition.value->getType();

    if (expr->is(NodeType::BinaryOp) && isEqOrRel(expr->as<node::BinaryOp>()->op))
        return condition.value;

    if (expr->is(NodeType::UnaryOp)) // explicit logical not
        if (auto op = expr->as<node::UnaryOp>()->op; op == TokenType::OpLNot || op == TokenType::OpNot)
            return condition.value;

    if (condTy->isFloatingPointTy()) // implicit expression
        return builder->CreateFCmpONE(condition.value,
                                      ConstantFP::get(*llvmctx->ctx, APFloat(0.0)),
                                      conditionName);
    else
        return builder->CreateICmpNE(condition.value,
                                     ConstantInt::get(condTy, 0),
                                     conditionName);
}

bool LLVMCodegenImpl::isBlockContains(const node::BlockPtr &block, NodeType nodeType) {
    const auto &stmts = block->stmts;
    return std::find_if(stmts.begin(), stmts.end(), [&nodeType](const node::NodePtr &stmt) {
        return stmt->is(nodeType);
    }) != stmts.end();
}

bool LLVMCodegenImpl::isBlockContainsJumpCond(const node::BlockPtr &block) {
    const auto &stmts = block->stmts;
    return std::find_if(stmts.begin(), stmts.end(), [](const node::NodePtr &node) {
        return node->isJumpCond();
    }) != stmts.end();
}

uint64_t LLVMCodegenImpl::sizeOf(const node::NodePtr &node) {
    const auto &dataLayout = llvmctx->module->getDataLayout();
    if (node->is(NodeType::Id)) {
        node->accept(*this, node);
        auto value = popValue();
        if (value.isType)
            return dataLayout.getTypeStoreSize(value.type);
        else {
            auto type = value.value->getType();
            if (type->isPointerTy())
                return dataLayout.getTypeAllocSize(type->getPointerElementType());
        }
    } else if (node->is(NodeType::UnaryOp)) {
        if (auto t = node->as<node::UnaryOp>(); t->op == TokenType::OpMul)
            return dataLayout.getPointerSize();
    } else if (node->is(NodeType::Decl) && node->as<node::Decl>()->type->isArray) {
        auto value = popValue();
        if (!value.isType) {
            auto type = value.value->getType();
            if (type->isPointerTy()) {
                return dataLayout.getTypeAllocSize(type->getPointerElementType());
            }
        }
    }

    throw ShineException(filename, node->pos, "Cannot get size of node: " + node->getNodeName());
}


void LLVMCodegenImpl::visit(const node::BlockPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    for (auto &stmt : node->stmts) {
        stmt->accept(*this, node);
        if (stmt->isJumpCond()) // ignore code after jump
            break;
    }
}

void LLVMCodegenImpl::visit(const node::IdPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    if (auto it = registeredTypes.find(node->val); it != registeredTypes.end()) {
        TypeInfo info;
        info.typeName = it->first;
        info.typeNode = invoker;
        pushValue(it->second, info);
    } else if (auto val = namedVariables.findNamedVariable(node->val); val != nullptr)
        pushValue(*val);
    else if (auto it = functions.find(node->val); it != functions.end())
        pushValue(it->second.value);
    else
        throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::IntPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    pushValue(builder->getInt64(node->val));
}

void LLVMCodegenImpl::visit(const node::FloatPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    float v = node->val;
    pushValue(llvm::ConstantFP::get(builder->getDoubleTy(), v));
}

void LLVMCodegenImpl::visit(const node::StringPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    //std::string name = getName(parent);
    //pushValue(builder->CreateGlobalStringPtr(node->val, name));
    pushValue(builder->CreateGlobalStringPtr(node->val));

    if (invoker == nullptr) return;

}

void LLVMCodegenImpl::visit(const node::BooleanPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    pushValue(builder->getInt1(node->val));
}

void LLVMCodegenImpl::visit(const node::CallPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    node->expr->accept(*this, node);
    auto expr = popValue();
    Value *func = expr.value;
    FunctionType *fnType = nullptr;

    if (auto type = func->getType(); type->isPointerTy()) {
        auto ptrType = type->getPointerElementType();
        if (ptrType->isPointerTy() && ptrType->getPointerElementType()->isFunctionTy()) {
            func = builder->CreateLoad(ptrType->getPointerElementType(), func);
            fnType = cast<FunctionType>(ptrType->getPointerElementType());
        } else if (ptrType->isFunctionTy())
            fnType = cast<FunctionType>(ptrType);
        else
            throw UnhandledNode(filename, node->expr);
    }


    std::vector<Value *> args;
    for (const auto &arg : node->args->vec) {
        arg->accept(*this, node);
        Value *value = popValue().value;
        auto const &vTy = value->getType();

        if (vTy->isPointerTy()) {
            if (!isa<ConstantExpr>(value)) {
                if (vTy->getPointerElementType()->isArrayTy()) {
                    std::vector<Value *> arr;
                    arr.push_back(builder->getInt64(0));
                    arr.push_back(builder->getInt64(0));
                    value = builder->CreateInBoundsGEP(vTy->getPointerElementType(), value, arr);
                } else
                    value = builder->CreateLoad(vTy->getPointerElementType(), value);
            }
        }
        if (fnType->isVarArg()) {
            if (vTy->isFloatTy()) // varag does not support 32-bit floats
                value = builder->CreateFPExt(value, builder->getDoubleTy());
        }
        args.push_back(value);
    }

    LLVMValue ret;

    if (node->expr->is(NodeType::Id)) {
        auto fname = node->expr->as<node::Id>()->val;
        if (fname == "alloca") // built-in
            ret = builder->CreateAlloca(fnType->getReturnType(), args[0]);
        else if (fname == "cast") {
            //builder->CreateBitCast()
        } else {
            ret = builder->CreateCall(fnType, func, args);
            ret.sign = expr.sign;
        }
    }
    pushValue(ret);

}

void LLVMCodegenImpl::visit(const node::UnaryOpPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    LLVMValue result{};
    if (node->op == TokenType::SizeOf) {
        auto sz = sizeOf(node->expr);
        result.value = builder->getInt64(sz);
        result.sign = false;
        pushValue(result);
        return;
    }

    node->expr->accept(*this, node);
    LLVMValue value = popValue();

    if (isa<Constant>(value.value)) {
        result.sign = value.sign;
        if (node->op == TokenType::OpBitAnd) // address-of
            result.value = builder->CreatePtrToInt(value.value, builder->getInt64Ty());
        else
            result = createUnaryOperation(node, value);
    } else {
        result.sign = value.sign;
        if (node->op == TokenType::OpBitAnd) // address-of
            result.value = builder->CreatePtrToInt(value.value, builder->getInt64Ty());
        else if (node->op == TokenType::OpMul) // indirection
            result.value = builder->CreateLoad(value.value->getType()->getPointerElementType(),value.value);
        else {
            auto ty = value.value->getType();
            LLVMValue lValue(builder->CreateLoad(ty, value.value), value.sign, value.constant);
            if (node->postfix)
                pushValue(lValue);
            result = createUnaryOperation(node, lValue);
            builder->CreateStore(result.value, value.value);
        }
    }
    if (!node->postfix)
        pushValue(result);
}

void LLVMCodegenImpl::visit(const node::BinaryOpPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    LLVMValue result{};
    node->left->accept(*this, node);
    LLVMValue leftValue = popValue();

    if (node->right == nullptr) {
        pushValue(leftValue); // todo: probably, not the best way to do it
        return;
    }

    pushValue(leftValue); //re-push for array & hashMap
    node->right->accept(*this, node);

    if (/*node->right->is(NodeType::Array) ||*/ node->right->is(NodeType::Hash))
        return;

    LLVMValue rightValue = popValue();
    popValue(); // pop leftValue

    if (node->op == TokenType::OpAssign) {
        if (leftValue.value->getType()->getPointerElementType()->isArrayTy()) {
            // node->right->is(NodeType::String)
            auto x = builder->CreateBitCast(leftValue.value, builder->getInt8PtrTy());
            //auto leftTy = leftValue.value->getType()->getPointerElementType();
            pushValue(leftValue);
            auto sz = sizeOf(node->left);
            //auto tySz = sz / leftTy->getArrayNumElements();
            auto align = 1; // todo: set correct alignment

            result = builder->CreateMemCpy(x, MaybeAlign(align), rightValue.value, MaybeAlign(align), sz);
        } else {
            result = createBinaryOperation(node, leftValue, rightValue);
            builder->CreateStore(result.value, leftValue.value);
        }
        //result.value = rightValue.value;
    } else {
        if (leftValue.typeInfo.typeNode != nullptr) {
            auto ty = leftValue.typeInfo.typeNode->as<node::Type>();
            if (ty->ptrLevel > 0 && ty->tname == "i8" && !rightValue.isType) {
                auto type = rightValue.value->getType();
                if (type->isPointerTy()) {
                    const auto &dataLayout = llvmctx->module->getDataLayout();
                    auto rrty = type->getPointerElementType();
                    uint64_t xx = dataLayout.getTypeAllocSize(rrty);
                    if (dataLayout.getTypeAllocSize(rrty) == 1) {
                        // TODO: strcmp
                        rrty->isVoidTy();
                        //auto mcmpFn = functions["memcmp"];
                        //builder->CreateCall(mcmpFn, )
                    }
                }
            }
        }
        LLVMValue lvalue = leftValue.value->getType()->isPointerTy() ? builder->CreateLoad(
                leftValue.value->getType()->getPointerElementType(), leftValue.value) : leftValue;
        LLVMValue rvalue = rightValue.value->getType()->isPointerTy() ? builder->CreateLoad(
                rightValue.value->getType()->getPointerElementType(), rightValue.value) : rightValue;
        lvalue.sign = leftValue.sign;
        lvalue.constant = leftValue.constant;

        auto isArr = lvalue.value->getType();
        auto isArr2 = rvalue.value->getType();

        result = createBinaryOperation(node, lvalue, rvalue);
        if (!isa<Constant>(leftValue.value) && (!shine::isEqOrRel(node->op)))
            builder->CreateStore(result.value, leftValue.value);
        pushValue(result);
    }

}

void LLVMCodegenImpl::visit(const node::FunctionPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Function *func = nullptr;

    if (auto it = functions.find(node->name); it != functions.end())
        func = cast<Function>(it->second.value.value);

    if (func == nullptr) // if no proto, create it
        func = createProto(node->name, node->type, node->params);

    BasicBlock *bblock = BasicBlock::Create(*llvmctx->ctx, "", func);
    builder->SetInsertPoint(bblock);
    namedVariables.pushLevel();

    int i = 0;
    for (auto &arg : func->args()) {
        AllocaInst *alloca = CreateEntryBlockAlloca(func, arg.getType(), arg.getName().str() + ".addr");

        // Store the initial value into the alloca.
        builder->CreateStore(&arg, alloca);

        // Add arguments to variable symbol table.
        namedVariables.insertVariable(arg.getName().str(),
                                      LLVMValue(alloca, functions.at(node->name).argSigns.at(i++)));
    }

    if (!isBlockContains(node->block, NodeType::Return)) // todo: add checks for non void functions
    {
        if (func->getReturnType()->isVoidTy())
            node->block->stmts.push_back(std::make_shared<node::Return>(nullptr, Position{}));
        else {
            const auto &lastNode = node->block->stmts.back();
            auto pos = lastNode->pos;
            throw ShineException(filename, pos, "Expected return statement");
        }
    }

    node->block->accept(*this, node);
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit(const node::ReturnPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Type *retType = builder->getCurrentFunctionReturnType();
    if (node->expr == nullptr) {
        if (!retType->isVoidTy())
            throw ShineException(filename, node->pos, "Expected return expression, got void");
        builder->CreateRetVoid();
        return;
    }
    node->expr->accept(*this, node);
    Value *ret = popValue().value;
    if (retType->isIntegerTy()) {
        bool isSigned = false;
        ret = builder->CreateIntCast(ret, retType, isSigned); // todo: check for signed
    } else if (retType->isPointerTy())
        ret = builder->CreatePointerCast(ret, retType);
    else if (retType->isFloatingPointTy())
        ret = builder->CreatePointerCast(ret, retType);
    builder->CreateRet(ret);
}

void LLVMCodegenImpl::visit(const node::DeclPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    for (const auto &val : node->vec) {
        if (val->nodeType != NodeType::Id)
            throw UnhandledNode(filename, val);

        auto name = val->as<node::Id>()->val;

        if (node->type == nullptr) // todo: inference type by value
            throw ShineException(filename, node->pos, "Type expected");

        node->type->accept(*this, node);
        auto t = popValue();
        Type *type = t.type;
        auto owner = builder->GetInsertBlock()->getParent();

        auto alloca = CreateEntryBlockAlloca(owner, type, name);

        bool _signed = false;
        int bwidth;
        isIntegerType(node->type->tname, bwidth, &_signed);
        LLVMValue v(alloca, _signed);

        v.typeInfo = t.typeInfo;

        namedVariables.insertVariable(name, v);

        pushValue(v);
    }
}

void LLVMCodegenImpl::visit(const node::WhilePtr &node, const node::NodePtr &invoker) // todo: add support for until
{
    SHINE_UNUSED(invoker);
    Function *owner = builder->GetInsertBlock()->getParent();

    BasicBlock *whileBB = BasicBlock::Create(*llvmctx->ctx, "whilecond", owner);
    BasicBlock *whileBodyBB = BasicBlock::Create(*llvmctx->ctx, "whilebody");
    BasicBlock *endBB = BasicBlock::Create(*llvmctx->ctx, "endwhile");

    namedVariables.pushLevel();

    loopContinueStack.push(whileBB);
    breakStack.push(endBB);

    builder->CreateBr(whileBB);
    builder->SetInsertPoint(whileBB);
    Value *condition = createCondition(node, node->expr, "whilecond");

    if (node->negate)
        builder->CreateCondBr(condition, endBB, whileBodyBB);
    else
        builder->CreateCondBr(condition, whileBodyBB, endBB);

    owner->getBasicBlockList().push_back(whileBodyBB);
    builder->SetInsertPoint(whileBodyBB);
    node->block->accept(*this, node);

    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(whileBB);

    owner->getBasicBlockList().push_back(endBB);
    builder->SetInsertPoint(endBB);
    loopContinueStack.pop();
    breakStack.pop();
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit(const node::RepeatPtr &node, const node::NodePtr &invoker) // todo: add support for until
{
    SHINE_UNUSED(invoker);
    Function *owner = builder->GetInsertBlock()->getParent();

    BasicBlock *repeatBodyBB = BasicBlock::Create(*llvmctx->ctx, "repeatbody", owner);
    BasicBlock *repeatCondBB = BasicBlock::Create(*llvmctx->ctx, "repeatcond");
    BasicBlock *endBB = BasicBlock::Create(*llvmctx->ctx, "endrepeat");

    namedVariables.pushLevel();

    loopContinueStack.push(repeatCondBB);
    breakStack.push(endBB);

    builder->CreateBr(repeatBodyBB);
    builder->SetInsertPoint(repeatBodyBB);
    node->block->accept(*this, node);

    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(repeatCondBB);

    owner->getBasicBlockList().push_back(repeatCondBB);
    builder->SetInsertPoint(repeatCondBB);
    Value *condition = createCondition(node, node->expr, "cmp");
    builder->CreateCondBr(condition, repeatBodyBB, endBB);

    owner->getBasicBlockList().push_back(endBB);
    builder->SetInsertPoint(endBB);
    loopContinueStack.pop();
    breakStack.pop();
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit(const node::ForPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Function *owner = builder->GetInsertBlock()->getParent();
    BasicBlock *forInitBB = BasicBlock::Create(*llvmctx->ctx, "forinit", owner);
    BasicBlock *forCondBB = BasicBlock::Create(*llvmctx->ctx, "forcond");
    BasicBlock *forBodyBB = BasicBlock::Create(*llvmctx->ctx, "forbody");
    BasicBlock *forStepBB = BasicBlock::Create(*llvmctx->ctx, "forstep");
    BasicBlock *forEndBB = BasicBlock::Create(*llvmctx->ctx, "forend");

    namedVariables.pushLevel();

    loopContinueStack.push(forCondBB);
    breakStack.push(forEndBB);

    builder->CreateBr(forInitBB);
    builder->SetInsertPoint(forInitBB);
    node->start->accept(*this, node);
    builder->CreateBr(forCondBB);

    owner->getBasicBlockList().push_back(forCondBB);
    builder->SetInsertPoint(forCondBB);
    Value *condition = createCondition(node, node->cond, "forcond");
    builder->CreateCondBr(condition, forBodyBB, forEndBB);

    owner->getBasicBlockList().push_back(forBodyBB);
    builder->SetInsertPoint(forBodyBB);
    node->block->accept(*this, node);
    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(forStepBB);

    owner->getBasicBlockList().push_back(forStepBB);
    builder->SetInsertPoint(forStepBB);
    node->step->accept(*this, node);
    builder->CreateBr(forCondBB);

    owner->getBasicBlockList().push_back(forEndBB);
    builder->SetInsertPoint(forEndBB);
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit(const node::IfPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Value *condition = createCondition(node, node->expr, "cmp");

    Function *owner = builder->GetInsertBlock()->getParent();

    BasicBlock *ifBB = BasicBlock::Create(*llvmctx->ctx, "if.then", owner);
    BasicBlock *endBB = BasicBlock::Create(*llvmctx->ctx, "if.end");
    BasicBlock *elseBB = nullptr;

    if (node->elseBlock != nullptr) {
        elseBB = BasicBlock::Create(*llvmctx->ctx, "if.else");
        if (node->negate)
            builder->CreateCondBr(condition, elseBB, ifBB);
        else
            builder->CreateCondBr(condition, ifBB, elseBB);
    } else {
        if (node->negate)
            builder->CreateCondBr(condition, endBB, ifBB);
        else
            builder->CreateCondBr(condition, ifBB, endBB);
    }

    // Emit if
    builder->SetInsertPoint(ifBB);
    namedVariables.pushLevel();
    node->block->accept(*this, node);
    namedVariables.popLevel();

    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(endBB);

    if (elseBB != nullptr) {
        owner->getBasicBlockList().push_back(elseBB);
        builder->SetInsertPoint(elseBB);

        namedVariables.pushLevel();
        node->elseBlock->accept(*this, node);
        namedVariables.popLevel();

        if (!isBlockContainsJumpCond(node->elseBlock))
            builder->CreateBr(endBB);
    }

    owner->getBasicBlockList().push_back(endBB);
    builder->SetInsertPoint(endBB);
}

void LLVMCodegenImpl::visit(const node::CasePtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Function *owner = builder->GetInsertBlock()->getParent();
    BasicBlock *swDefaultBB = BasicBlock::Create(*llvmctx->ctx, "sw.default");
    BasicBlock *swEpilogBB = BasicBlock::Create(*llvmctx->ctx, "sw.epilog");
    node->expr->accept(*this, node);
    auto caseExpr = popValue();
    llvm::SwitchInst *theSwitch = builder->CreateSwitch(builder->CreateLoad(caseExpr.value->getType()->getPointerElementType(), caseExpr.value), swDefaultBB);
    std::vector<std::pair<node::BlockPtr, BasicBlock *>> swCases;


    breakStack.push(swEpilogBB);

    for (const auto &when : node->whenStmts) {
        BasicBlock *bb = BasicBlock::Create(*llvmctx->ctx, "sw.bb");

        for (const auto &expr : when->exprs) {
            expr->accept(*this, node);
            auto value = popValue();
            Value *onVal = IntCast(caseExpr, value);
            theSwitch->addCase(cast<ConstantInt>(onVal), bb);
        }

        swCases.emplace_back(when->block, bb);
    }

    for (const auto &[body, bb] : swCases) {
        owner->getBasicBlockList().push_back(bb);

        builder->SetInsertPoint(bb);
        body->accept(*this, node);
        popValue(); // ignored value
        if (!isBlockContainsJumpCond(body))
            builder->CreateBr(swEpilogBB);
    }

    owner->getBasicBlockList().push_back(swDefaultBB);
    builder->SetInsertPoint(swDefaultBB);
    if (node->elseBlock) {
        node->elseBlock->accept(*this, node);
        popValue(); // ignored value
        if (!isBlockContainsJumpCond(node->elseBlock))
            builder->CreateBr(swEpilogBB);
    }

    owner->getBasicBlockList().push_back(swEpilogBB);
    builder->SetInsertPoint(swEpilogBB);
}

void LLVMCodegenImpl::visit(const node::SubscriptPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    node->left->accept(*this, node);
    auto left = popValue().value;
    node->right->accept(*this, node);
    auto idx = popValue().value;

    auto ltype = left->getType();
    auto isPtrToPtr = ltype->isPointerTy() && ltype->getPointerElementType()->isPointerTy();


    if (!isPtrToPtr) {
        std::vector<Value *> arr;
        arr.push_back(builder->getInt64(0));
        arr.push_back(builder->CreateZExt(idx, builder->getInt64Ty()));
        pushValue(builder->CreateInBoundsGEP(left->getType()->getPointerElementType(), left, arr));
    } else {
        auto load = builder->CreateLoad(left->getType()->getPointerElementType(), left);
        pushValue(builder->CreateInBoundsGEP(load->getType()->getPointerElementType(), load, idx));
    }
}

void LLVMCodegenImpl::visit(const node::TypePtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Type *type = nullptr;
    TypeInfo info;
    info.typeName = node->tname;
    info.typeNode = node;

    if (node->tname == "void") {
        if (node->ptrLevel > 0)
            type = builder->getInt8PtrTy();
        else
            type = builder->getVoidTy();
    } else if (auto it = registeredTypes.find(node->tname); it != registeredTypes.end())
        type = it->second;
    else
        throw UnhandledNode(filename, node);

    if (node->isFunc) {
        auto args = getLLVMParams(node->funParams);
        type = FunctionType::get(type, args.params, args.isVarArg)->getPointerTo();
        pushValue(type, info);
        return;
    }

    for (int i = node->tname == "void" ? 1 : 0; i < node->ptrLevel; ++i) // get pointer or pointer to N pointer
        type = type->getPointerTo();

    if (node->isArray) {
        unsigned elems = node->arrSize;
        //type = llvm::PointerType::getUnqual(ArrayType::get(type, elems));
        type = ArrayType::get(type, elems);
    }
    pushValue(type, info);
}


void LLVMCodegenImpl::visit(const node::LetPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    for (const auto &v : node->vec) {
        v->accept(*this, node);
    }
}

void LLVMCodegenImpl::visit(const node::ConstPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    for (const auto &v : node->vec) {
        v->accept(*this, node);
        for (const auto &l : v->left->as<node::Decl>()->vec)
            namedVariables.top().at(l->val).constant = true;
    }
}

/*void LLVMCodegenImpl::visit(const node::ExternPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    Function *func = createProto(node->name, node->type, node->params);
    //func->addAttribute(1, Attribute::ReadOnly);
    //func->addAttribute(1, Attribute::NoCapture);
}*/

void LLVMCodegenImpl::visit(const node::ContinuePtr &/*node*/, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    builder->CreateBr(loopContinueStack.top());
}

void LLVMCodegenImpl::visit(const node::BreakPtr &/*node*/, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    builder->CreateBr(breakStack.top());
}

void LLVMCodegenImpl::visit(const node::SlotPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    node->left->accept(*this, node);
    //node->right->accept(*this, node);
    //auto right = popValue();
    Value *left = popValue().value;

    Type *leftPtrT = left->getType()->getPointerElementType();

    if (leftPtrT->isStructTy()) {
        auto memberName = node->right->as<node::Id>()->val;
        auto typeIdx = structTypeIdx.at(leftPtrT->getStructName().str()).at(memberName);
        Value *argValuePtr = builder->CreateStructGEP(left->getType()->getPointerElementType(), left, typeIdx, left->getName() + "." + memberName);
        pushValue(argValuePtr);
        return;
    }

    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::ArrayPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    auto left = popValue();

    if (left.constant)
        throw ShineException(filename, node->pos, "cannot assign to variable with constant qualifier");

    std::vector<Constant *> arr;
    for (const auto &v : node->vals) {
        v->accept(*this, node);
        auto rval = popValue();

        arr.push_back(cast<Constant>(IntCast(left, rval)));
    }

    auto arrType = cast<ArrayType>(left.value->getType()->getPointerElementType());

    uint64_t numElements = arrType->getNumElements();

    if (numElements == 0) // recreate array with proper size
    {
        numElements = arr.size();
        /*auto T = left.value->getType()->getPointerElementType()->getArrayElementType();
        auto arrT = ArrayType::get(T, arr.size());
        auto name = left.value->getName().str();

        auto newVal = CreateEntryBlockAlloca(builder->GetInsertBlock()->getParent(), arrT, "");
        left.value->replaceAllUsesWith(newVal);
        left.value = newVal;*/
    }

    arr.resize(numElements, cast<Constant>(IntCast(left, builder->getInt8(0))));

    ArrayType *arrayType = ArrayType::get(arrType->getArrayElementType(), numElements);
    Constant *init = ConstantArray::get(arrayType, arr);
    auto v = new GlobalVariable(*llvmctx->module, init->getType(), true, GlobalValue::PrivateLinkage, init);
    std::vector<Value *> arr2;
    arr2.push_back(builder->getInt64(0));
    arr2.push_back(builder->getInt64(0));
/*#if LLVM_VERSION_MAJOR > 8
    builder->CreateMemCpy(left.value, MaybeAlign(1), builder->CreateInBoundsGEP(v->getType()->getPointerElementType(), v, arr2), MaybeAlign(1), numElements);
#else
    builder->CreateMemCpy(left.value, 1, builder->CreateInBoundsGEP(v, arr2), 1, numElements);
#endif*/
    pushValue(left);
    pushValue(builder->CreateInBoundsGEP(v->getType()->getPointerElementType(), v, arr2));
}

void LLVMCodegenImpl::visit(const node::HashPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::StructPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    std::vector<Type *> elements;
    std::map<std::string, int> types;
    int idx = 0;
    for (const auto &field : node->fields) {
        field->type->accept(*this, node);
        auto type = popValue().type;
        for (const auto &id : field->vec) {
            elements.push_back(type);
            types.emplace(id->val, idx++);
        }
    }

    registerType(node->name, StructType::create(*llvmctx->ctx, elements, node->name));
    structTypeIdx.emplace(node->name, types);
}

void LLVMCodegenImpl::visit(const node::VaArgPtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::UsePtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::ModulePtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(const node::ScopePtr &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw UnhandledNode(filename, node);
}

/*void LLVMCodegenImpl::visit(node::ArgsPtr const &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw shine::UnhandledNode(filename, node);
}*/

void LLVMCodegenImpl::visit(node::ProtoPtr const &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    /*Function *func = */createProto(node->name, node->type, node->params);
    //func->addAttribute(1, Attribute::ReadOnly);
    //func->addAttribute(1, Attribute::NoCapture);
}

void LLVMCodegenImpl::visit(node::WhenPtr const &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw shine::UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit(node::HashPairPtr const &node, const node::NodePtr &invoker) {
    SHINE_UNUSED(invoker);
    throw shine::UnhandledNode(filename, node);
}
