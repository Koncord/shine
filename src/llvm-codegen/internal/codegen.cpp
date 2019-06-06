#include <utils/exception.hpp>
#include <llvm/ADT/StringExtras.h>
#include "codegen.hpp"

using namespace shine;
using namespace llvm;

LLVMCodegenImpl::LLVMCodegenImpl(const node::NodePtr &root, LLVMCtx *llvmctx, std::string filename) : llvmctx(llvmctx)
{
    namedVariables.pushLevel();
    builder = std::make_unique<IRBuilder<>>(*llvmctx->ctx);

    this->filename = std::move(filename);
    this->root = root;

    registerTypes();
    createProto("alloca",
                std::make_shared<node::Type>("void", true, Position {}), {
                        std::make_shared<node::Decl>((std::vector<node::IdPtr>) {
                                std::make_shared<node::Id>("sz", Position {})
                        }, std::make_shared<node::Type>("i64", true, Position {}), Position {})
                }
    );

    visit(root);
}

void LLVMCodegenImpl::registerTypes()
{
    registeredTypes.emplace(std::make_pair("void", false), builder->getVoidTy());
    registeredTypes.emplace(std::make_pair("void", true), builder->getInt8PtrTy());

    auto insert = [this](const std::string &name, Type *type) {
        registeredTypes.emplace(std::make_pair(name, false), type);
        registeredTypes.emplace(std::make_pair(name, true), type->getPointerTo());
    };

    insert("double", builder->getDoubleTy());
    insert("float", builder->getFloatTy());
    insert("boolean", builder->getInt1Ty());
    insert("string", builder->getInt8PtrTy());

    for(uint32_t bitwidth = 8; bitwidth <= 64; bitwidth <<= 1) // integer types
    {
        std::string bw = std::to_string(bitwidth);
        insert("u" + bw, builder->getIntNTy(bitwidth));
        insert("i" + bw, builder->getIntNTy(bitwidth));
    }
}

AllocaInst *LLVMCodegenImpl::CreateEntryBlockAlloca(Function *function, Type *type, const std::string &varName)
{
    IRBuilder<> TmpB(&function->getEntryBlock(), function->getEntryBlock().begin());
    return TmpB.CreateAlloca(type, nullptr, varName);
}

std::string LLVMCodegenImpl::getName(const node::NodePtr &node)
{
    std::string name;
    if (node->is(NodeType::BinaryOp))
    {
        const auto &binNode = node->as<node::BinaryOp>();
        if (binNode->left->is(NodeType::Id))
            name = binNode->left->as<node::Id>()->val;
        else
            name = binNode->left->as<node::Decl>()->vec.at(0)->as<node::Id>()->val;
    }
    return name;
}

LLVMValue LLVMCodegenImpl::popValue()
{
    return valStack.pop();
}

void LLVMCodegenImpl::pushValue(LLVMValue value)
{
    valStack.push(value);
}

LLVMValue *NamedVariables::findNamedVariable(const std::string &name) // reverse search of variable
{
    int level = namedVariablesStack.size() - 1;
    for (auto it = namedVariablesStack.rbegin(); it != namedVariablesStack.rend(); ++it)
    {
        auto vname = name + "_" + std::to_string(level);
        if (auto v = std::find_if(it->begin(), it->end(), [&vname](const auto &val){return val.first == vname;}); v != it->end())
            return &v->second;
        level -= 1;
    }
    return nullptr;
}

void NamedVariables::pushLevel()
{
    namedVariablesStack.push(NamedVariablesT());
    varLevel += 1;
}

void NamedVariables::popLevel()
{
    namedVariablesStack.vpop();
    varLevel -= 1;
}

void NamedVariables::insertVariable(std::string name, LLVMValue val)
{
    name += "_" + std::to_string(varLevel);
    top().emplace(name, val);
}

NamedVariables::NamedVariablesT &NamedVariables::top()
{
    return namedVariablesStack.top();
}

const NamedVariables::NamedVariablesT &NamedVariables::top() const
{
    return namedVariablesStack.top();
}

bool LLVMCodegenImpl::isIntegerType(std::string ref, int &bitwidth, bool *isSigned)
{
    if (ref[0] == 'i' || ref[0] == 'u')
    {
        if (isSigned != nullptr)
            *isSigned = ref[0] == 'i';
        if (llvm::to_integer(StringRef(&ref[1]), bitwidth))
            return (bitwidth == 8 || bitwidth == 16 || bitwidth == 32 || bitwidth == 64);
    }
    return false;
}

Function *LLVMCodegenImpl::createProto(
        const std::string &name,
        const node::TypePtr &nodeType,
        const std::vector<node::NodePtr> &params
)
{
    std::vector<Type *> args;
    std::vector<std::string> names;

    bool isVarArg = false;
    int ignored;

    bool sign = false;
    isIntegerType(nodeType->tname, ignored, &sign);
    functionRetSign[name] = sign;

    auto &fArgSigns = functionArgSigns[name];

    for (const auto &arg : params)
    {
        if (arg->is(NodeType::VaArg))
        {
            isVarArg = true;
            break;
        }
        if (!arg->is(NodeType::Decl))
            throw UnhandledNode(filename, arg);
        const auto &declNode = arg->as<node::Decl>();
        visit(declNode->type);
        args.push_back(popValue().type);
        names.push_back(declNode->vec[0]->val);
        sign = false;
        isIntegerType(declNode->type->tname, ignored, &sign);
        fArgSigns.push_back(sign);
    }

    visit(nodeType);
    llvm::Type *retType = popValue().type;

    FunctionType *fnType = FunctionType::get(retType, args, isVarArg);
    auto linkage = Function::ExternalLinkage;
    auto func = Function::Create(fnType, linkage, name, llvmctx->module.get());

    int idx = 0;
    for (auto &arg : func->args())
        arg.setName(names[idx++]);
    functions[name] = func;

    return func;
}

Value *LLVMCodegenImpl::createCondition(
        const node::NodePtr &node,
        const node::NodePtr &expr,
        const std::string &conditionName
)
{
    visit(expr);
    LLVMValue condition = popValue();

    if (condition.value->getType()->isPointerTy())
        condition.value = builder->CreateLoad(condition.value);

    bool isFp = condition.value->getType()->isFloatingPointTy();

    Value *const0 = nullptr;

    if(isFp)
        const0 = ConstantFP::get(*llvmctx->ctx, APFloat(0.0));
    else
        const0 = ConstantInt::get(condition.value->getType(), 0);

    if (expr->is(NodeType::BinaryOp) && isEqOrRel(expr->as<node::BinaryOp>()->op))
        return condition.value;

    if (expr->is(NodeType::UnaryOp)) // explicit logical not
        if (auto op = expr->as<node::UnaryOp>()->op; op == TokenType::OpLNot || op == TokenType::OpNot)
            return condition.value;

    if (condition.value->getType()->isFloatingPointTy()) // implicit expression
        return builder->CreateFCmpONE(condition.value, const0, conditionName);
    else
        return builder->CreateICmpNE(condition.value, const0, conditionName);
}

bool LLVMCodegenImpl::isBlockContains(const node::BlockPtr &block, NodeType nodeType)
{
    const auto &stmts = block->stmts;
    return std::find_if(stmts.begin(), stmts.end(), [&nodeType](const node::NodePtr &stmt) {
        return stmt->is(nodeType);
    }) != stmts.end();
}

bool LLVMCodegenImpl::isBlockContainsJumpCond(const node::BlockPtr &block)
{
    const auto &stmts = block->stmts;
    return std::find_if(stmts.begin(), stmts.end(), [](const node::NodePtr &node) {
        return node->isJumpCond();
    }) != stmts.end();
}


void LLVMCodegenImpl::visit_block(const node::BlockPtr &node)
{
    for (auto &stmt : node->stmts)
    {
        visit(stmt);
        if (stmt->isJumpCond()) // ignore code after jump
            break;
    }
}

void LLVMCodegenImpl::visit_id(const node::IdPtr &node)
{
    if (auto val = namedVariables.findNamedVariable(node->val); val != nullptr)
        pushValue(*val);
    else if (auto it = functions.find(node->val); it != functions.end())
        pushValue(it->second);
    else
        throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit_int(const node::IntPtr &node)
{
    pushValue(builder->getInt64(node->val));
}

void LLVMCodegenImpl::visit_float(const node::FloatPtr &node)
{
    float v = node->val;
    pushValue(llvm::ConstantFP::get(builder->getDoubleTy(), v));
}

void LLVMCodegenImpl::visit_string(const node::StringPtr &node)
{
    //std::string name = getName(parent);
    //pushValue(builder->CreateGlobalStringPtr(node->val, name));
    pushValue(builder->CreateGlobalStringPtr(node->val));
}

void LLVMCodegenImpl::visit_boolean(const node::BooleanPtr &node)
{
    pushValue(builder->getInt1(node->val));
}

void LLVMCodegenImpl::visit_call(const node::CallPtr &node)
{
    visit(node->expr);
    auto func = cast<Function>(popValue().value);

    if (!func->getType()->isPointerTy() || !func->getType()->getPointerElementType()->isFunctionTy())
    {
        throw UnhandledNode(filename, node->expr);
    }

    std::vector<Value *> args;
    for (const auto &arg : node->args->vec)
    {
        visit(arg);
        Value *value = popValue().value;

        if (value->getType()->isPointerTy())
        {
            if (!isa<ConstantExpr>(value))
            {
                if (value->getType()->getPointerElementType()->isArrayTy())
                {
                    std::vector<Value*> arr;
                    arr.push_back(builder->getInt64(0));
                    arr.push_back(builder->getInt64(0));
                    value = builder->CreateInBoundsGEP(value, arr);
                }
                else
                    value = builder->CreateLoad(value);
            }
        }
        if (func->isVarArg())
        {
            if (value->getType()->isFloatTy()) // varag does not support 32-bit floats
                value = builder->CreateFPExt(value, builder->getDoubleTy());
        }
        args.push_back(value);
    }

    LLVMValue ret;

    if (node->expr->is(NodeType::Id))
    {
        auto fname = node->expr->as<node::Id>()->val;
        if (fname == "alloca") // built-in
            ret = builder->CreateAlloca(func->getReturnType(), args[0]);
        else if (fname == "cast")
        {
            //builder->CreateBitCast()
        }
        else
        {
            ret = builder->CreateCall(func, args);
            ret.sign = functionRetSign.at(fname);
        }
    }
    pushValue(ret);

}

void LLVMCodegenImpl::visit_unary_op(const node::UnaryOpPtr &node)
{
    visit(node->expr);
    LLVMValue value = popValue();
    LLVMValue result {};
    if (isa<Constant>(value.value))
    {
        result = createUnaryOperation(node, value);
    }
    else
    {
        result.sign = value.sign;
        if (node->op == TokenType::OpBitAnd) // address-of
            result.value = builder->CreatePtrToInt(value.value, builder->getInt64Ty());
        else if (node->op == TokenType::OpMul) // indirection
            result.value = builder->CreateLoad(value.value);
        else
        {
            LLVMValue lValue(builder->CreateLoad(value.value), value.sign, value.constant);
            if (node->postfix)
                pushValue(lValue);
            result = createUnaryOperation(node, lValue);
            builder->CreateStore(result.value, value.value);
        }
    }
    if (!node->postfix)
        pushValue(result);
}

void LLVMCodegenImpl::visit_binary_op(const node::BinaryOpPtr &node)
{
    LLVMValue result {};
    visit(node->left);
    LLVMValue leftValue = popValue();

    if (node->right == nullptr)
    {
        pushValue(leftValue); // todo: probably, not the best way to do it
        return;
    }

    pushValue(leftValue); //re-push for array & hashMap
    visit(node->right);

    if (node->right->is(NodeType::Array) || node->right->is(NodeType::Hash))
        return;

    LLVMValue rightValue = popValue();
    popValue(); // pop leftValue

    if (node->op == TokenType::OpAssign)
    {
        result = createBinaryOperation(node, leftValue, rightValue);
        builder->CreateStore(result.value, leftValue.value);
    }
    else
    {
        LLVMValue lvalue = leftValue.value->getType()->isPointerTy() ? builder->CreateLoad(leftValue.value) : leftValue;
        LLVMValue rvalue = rightValue.value->getType()->isPointerTy() ? builder->CreateLoad(rightValue.value)
                                                                      : rightValue;
        lvalue.sign = leftValue.sign;
        lvalue.constant = leftValue.constant;

        result = createBinaryOperation(node, lvalue, rvalue);
        if (!isa<Constant>(leftValue.value) && (!shine::isEqOrRel(node->op)))
            builder->CreateStore(result.value, leftValue.value);
        pushValue(result);
    }

}

void LLVMCodegenImpl::visit_function(const node::FunctionPtr &node)
{
    Function *func = functions[node->name]; // check proto

    if (func == nullptr) // if no proto, create it
        func = createProto(node->name, node->type, node->params);

    BasicBlock *bblock = BasicBlock::Create(*llvmctx->ctx, "", func);
    builder->SetInsertPoint(bblock);
    namedVariables.pushLevel();

    int i = 0;
    for (auto &arg : func->args())
    {
        AllocaInst *alloca = CreateEntryBlockAlloca(func, arg.getType(), arg.getName().str() + ".addr");

        // Store the initial value into the alloca.
        builder->CreateStore(&arg, alloca);

        // Add arguments to variable symbol table.
        namedVariables.insertVariable(arg.getName(), LLVMValue(alloca, functionArgSigns.at(node->name).at(i++)));
    }

    if (!isBlockContains(node->block, NodeType::Return)) // todo: add checks for non void functions
    {
        if (func->getReturnType()->isVoidTy())
            node->block->stmts.push_back(std::make_shared<node::Return>(nullptr, Position {}));
        else
        {
            const auto &lastNode = node->block->stmts.back();
            auto pos = lastNode->pos;
            throw ShineException(filename, pos, "Expected return statement");
        }
    }

    visit(node->block);
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit_return(const node::ReturnPtr &node)
{
    Type *retType = builder->getCurrentFunctionReturnType();
    if (node->expr == nullptr)
    {
        if (!retType->isVoidTy())
            throw ShineException(filename, node->pos, "Expected return expression, got void");
        builder->CreateRetVoid();
        return;
    }
    visit(node->expr);
    Value *ret = popValue().value;
    if (retType->isIntegerTy())
    {
        bool isSigned = false;
        ret = builder->CreateIntCast(ret, retType, isSigned); // todo: check for signed
    }
    else if (retType->isPointerTy())
        ret = builder->CreatePointerCast(ret, retType);
    else if (retType->isFloatingPointTy())
        ret = builder->CreatePointerCast(ret, retType);
    builder->CreateRet(ret);
}

void LLVMCodegenImpl::visit_decl(const node::DeclPtr &node)
{
    for (const auto &val : node->vec)
    {
        if (val->nodeType != NodeType::Id)
            throw UnhandledNode(filename, val);

        auto name = val->as<node::Id>()->val;

        if (node->type == nullptr) // todo: inference type by value
            throw ShineException(filename, node->pos, "Type expected");

        visit(node->type);
        Type *type = popValue().type;
        auto owner = builder->GetInsertBlock()->getParent();

        auto alloca = CreateEntryBlockAlloca(owner, type, name);

        bool _signed = false;
        int bwidth;
        isIntegerType(node->type->tname, bwidth, &_signed);
        LLVMValue v(alloca, _signed);

        namedVariables.insertVariable(name, v);

        pushValue(v);
    }
}

void LLVMCodegenImpl::visit_while(const node::WhilePtr &node) // todo: add support for until
{
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
    visit(node->block);

    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(whileBB);

    owner->getBasicBlockList().push_back(endBB);
    builder->SetInsertPoint(endBB);
    loopContinueStack.pop();
    breakStack.pop();
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit_repeat(const node::RepeatPtr &node) // todo: add support for until
{
    Function *owner = builder->GetInsertBlock()->getParent();

    BasicBlock *repeatBodyBB = BasicBlock::Create(*llvmctx->ctx, "repeatbody", owner);
    BasicBlock *repeatCondBB = BasicBlock::Create(*llvmctx->ctx, "repeatcond");
    BasicBlock *endBB = BasicBlock::Create(*llvmctx->ctx, "endrepeat");

    namedVariables.pushLevel();

    loopContinueStack.push(repeatCondBB);
    breakStack.push(endBB);

    builder->CreateBr(repeatBodyBB);
    builder->SetInsertPoint(repeatBodyBB);
    visit(node->block);

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

void LLVMCodegenImpl::visit_for(const node::ForPtr &node)
{
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
    visit(node->start);
    builder->CreateBr(forCondBB);

    owner->getBasicBlockList().push_back(forCondBB);
    builder->SetInsertPoint(forCondBB);
    Value *condition = createCondition(node, node->cond, "forcond");
    builder->CreateCondBr(condition, forBodyBB, forEndBB);

    owner->getBasicBlockList().push_back(forBodyBB);
    builder->SetInsertPoint(forBodyBB);
    visit(node->block);
    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(forStepBB);

    owner->getBasicBlockList().push_back(forStepBB);
    builder->SetInsertPoint(forStepBB);
    visit(node->step);
    builder->CreateBr(forCondBB);

    owner->getBasicBlockList().push_back(forEndBB);
    builder->SetInsertPoint(forEndBB);
    namedVariables.popLevel();
}

void LLVMCodegenImpl::visit_if(const node::IfPtr &node)
{
    Value *condition = createCondition(node, node->expr, "cmp");

    Function *owner = builder->GetInsertBlock()->getParent();

    BasicBlock *ifBB = BasicBlock::Create(*llvmctx->ctx, "if.then", owner);
    BasicBlock *endBB = BasicBlock::Create(*llvmctx->ctx, "if.end");
    BasicBlock *elseBB = nullptr;

    if (node->elseBlock != nullptr)
    {
        elseBB = BasicBlock::Create(*llvmctx->ctx, "if.else");
        if(node->negate)
            builder->CreateCondBr(condition, elseBB, ifBB);
        else
            builder->CreateCondBr(condition, ifBB, elseBB);
    }
    else
    {
        if(node->negate)
            builder->CreateCondBr(condition, endBB, ifBB);
        else
            builder->CreateCondBr(condition, ifBB, endBB);
    }

    // Emit if
    builder->SetInsertPoint(ifBB);
    namedVariables.pushLevel();
    visit(node->block);
    namedVariables.popLevel();

    if (!isBlockContainsJumpCond(node->block))
        builder->CreateBr(endBB);

    if (elseBB != nullptr)
    {
        owner->getBasicBlockList().push_back(elseBB);
        builder->SetInsertPoint(elseBB);

        namedVariables.pushLevel();
        visit(node->elseBlock);
        namedVariables.popLevel();

        if (!isBlockContainsJumpCond(node->elseBlock))
            builder->CreateBr(endBB);
    }

    owner->getBasicBlockList().push_back(endBB);
    builder->SetInsertPoint(endBB);
}

void LLVMCodegenImpl::visit_case(const node::CasePtr &node)
{
    Function *owner = builder->GetInsertBlock()->getParent();
    BasicBlock *swDefaultBB = BasicBlock::Create(*llvmctx->ctx, "sw.default");
    BasicBlock *swEpilogBB = BasicBlock::Create(*llvmctx->ctx, "sw.epilog");
    visit(node->expr);
    auto caseExpr = popValue();
    llvm::SwitchInst *theSwitch = builder->CreateSwitch(builder->CreateLoad(caseExpr.value), swDefaultBB);
    std::vector<std::pair<node::BlockPtr, BasicBlock *>> swCases;


    breakStack.push(swEpilogBB);

    for(const auto &when : node->whenStmts)
    {
        BasicBlock *bb = BasicBlock::Create(*llvmctx->ctx, "sw.bb");

        for (const auto &expr : when->exprs)
        {
            visit(expr);
            auto value = popValue();
            Value *onVal = IntCast(caseExpr, value);
            theSwitch->addCase(cast<ConstantInt>(onVal), bb);
        }

        swCases.emplace_back(when->block, bb);
    }

    for(const auto &[body, bb] : swCases)
    {
        owner->getBasicBlockList().push_back(bb);

        builder->SetInsertPoint(bb);
        visit(body);
        popValue(); // ignored value
        if(!isBlockContainsJumpCond(body))
            builder->CreateBr(swEpilogBB);
    }

    owner->getBasicBlockList().push_back(swDefaultBB);
    builder->SetInsertPoint(swDefaultBB);
    if (node->elseBlock)
    {
        visit(node->elseBlock);
        popValue(); // ignored value
        if(!isBlockContainsJumpCond(node->elseBlock))
            builder->CreateBr(swEpilogBB);
    }

    owner->getBasicBlockList().push_back(swEpilogBB);
    builder->SetInsertPoint(swEpilogBB);
}

void LLVMCodegenImpl::visit_subscript(const node::SubscriptPtr &node)
{
    visit(node->left);
    auto left = popValue();
    visit(node->right);
    auto idx = popValue();

    auto ltype = left.value->getType();
    auto isStackArr = ltype->isPointerTy() ? ltype->getPointerElementType()->isArrayTy() : false;

    if (isStackArr)
    {
        std::vector<Value*> arr;
        arr.push_back(builder->getInt64(0));
        arr.push_back(idx.value);
        pushValue(builder->CreateInBoundsGEP(left.value, arr));
    }
    else
    {
        std::vector<Value*> arr;
        arr.push_back(builder->getInt64(0));
        arr.push_back(idx.value);
        pushValue(builder->CreateInBoundsGEP(builder->CreateLoad(left.value), idx.value));
    }
}

void LLVMCodegenImpl::visit_type(const node::TypePtr &node)
{
    if (auto it = registeredTypes.find(std::make_pair(node->tname, node->isPtr)); it != registeredTypes.end())
    {
        auto type = it->second;
        if (node->arrSize != 0)
        {
            unsigned elems = node->arrSize;
            //type = llvm::PointerType::getUnqual(ArrayType::get(type, elems));
            type = ArrayType::get(type, elems);
        }
        pushValue(type);
    }
    else
        throw UnhandledNode(filename, node);
}


void LLVMCodegenImpl::visit_let(const node::LetPtr &node)
{
    for (const auto &v : node->vec)
    {
        visit(v);
    }
}

void LLVMCodegenImpl::visit_const(const node::ConstPtr &node)
{
    for (const auto &v : node->vec)
    {
        visit(v);
        for (const auto &l : v->left->as<node::Decl>()->vec)
            namedVariables.top().at(l->val).constant = true;
    }
}

void LLVMCodegenImpl::visit_extern(const node::ExternPtr &node)
{
    Function *func = createProto(node->name, node->type, node->params);
    //func->addAttribute(1, Attribute::ReadOnly);
    //func->addAttribute(1, Attribute::NoCapture);
}

void LLVMCodegenImpl::visit_continue(const node::ContinuePtr &node)
{
    builder->CreateBr(loopContinueStack.top());
}

void LLVMCodegenImpl::visit_break(const node::BreakPtr &node)
{
    builder->CreateBr(breakStack.top());
}

void LLVMCodegenImpl::visit_slot(const node::SlotPtr &node)
{
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit_array(const node::ArrayPtr &node)
{
    auto left = popValue();

    if (left.constant)
        throw ShineException(filename, node->pos, "cannot assign to variable with constant qualifier");

    throw UnhandledNode(filename, node);

    std::vector<Constant *> arr;
    for(const auto &v : node->vals)
    {
        visit(v);
        auto rval = popValue();

        arr.push_back(cast<Constant>(IntCast(left, rval)));
    }

    auto t = left.value->getType()->getPointerElementType()->getArrayElementType();
    auto GV = ConstantArray::get(ArrayType::get(t, arr.size()), arr);
    /*Constant *Zero = ConstantInt::get(Type::getInt32Ty(*llvmctx->ctx), 0);
    Constant *Indices[] = {Zero, Zero};
    ConstantExpr::getInBoundsGetElementPtr(t, GV, Indices)*/
    //pushValue(GV);
    namedVariables.insertVariable("arr", GV);
}

void LLVMCodegenImpl::visit_hash(const node::HashPtr &node)
{
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit_struct(const node::StructPtr &node)
{
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit_vaarg(const node::VaArgPtr &node)
{
    throw UnhandledNode(filename, node);
}

void LLVMCodegenImpl::visit_use(const node::UsePtr &node)
{
    throw UnhandledNode(filename, node);
}
