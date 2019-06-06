#include <lexer/token.hpp>
#include <types.hpp>
#include <ast/ast.hpp>
#include <utils/exception.hpp>
#include <llvm/IR/Value.h>
#include "llvm/IR/IRBuilder.h"
#include "codegen.hpp"

using namespace llvm;

namespace shine
{
    Value *LLVMCodegenImpl::IntCast(
            const LLVMValue &leftValue,
            const LLVMValue &rightValue
    )
    {
        Value *lval = leftValue.value;
        Value *rval = rightValue.value;
        Type *lRealType = lval->getType()->isPointerTy() ? lval->getType()->getPointerElementType() : lval->getType();

        if (lRealType->isArrayTy())
            lRealType = lRealType->getArrayElementType();

        if (rval->getType()->isFloatingPointTy())
            rval = leftValue.sign ? builder->CreateFPToSI(rval, lRealType)
                                  : builder->CreateFPToUI(rval, lRealType);
        else if (!rval->getType()->isPointerTy())
        {
            int rbitwidth = rval->getType()->getIntegerBitWidth();
            int lbitwidth = lRealType->getIntegerBitWidth();
            if (lbitwidth > rbitwidth)
                rval = rightValue.sign ? builder->CreateSExt(rval, lRealType) // copy sign
                                       : builder->CreateZExt(rval, lRealType); // zero sign
            else if (lbitwidth < rbitwidth)
                rval = builder->CreateTrunc(rval, lRealType);
        }
        return rval;
    }

    llvm::Value *LLVMCodegenImpl::FPCast(const LLVMValue &leftValue, const LLVMValue &rightValue)
    {
        Value *lval = leftValue.value;
        Value *rval = rightValue.value;
        Type *lRealType = lval->getType()->isPointerTy() ? lval->getType()->getPointerElementType() : lval->getType();

        if (rval->getType()->isIntegerTy())
        {
            rval = rightValue.sign ? builder->CreateSIToFP(rval, lRealType)
                                   : builder->CreateUIToFP(rval, lRealType);
        }
        else
        {
            bool isRDouble = rval->getType()->isDoubleTy();
            bool isLDouble = lRealType->isDoubleTy();
            if (isLDouble && !isRDouble)
                rval = builder->CreateFPExt(rval, lRealType);
            else if(!isLDouble && isRDouble)
                rval = builder->CreateFPTrunc(rval, lRealType);
        }
        return rval;
    }

    LLVMValue LLVMCodegenImpl::createUnaryOperation(
            const node::UnaryOpPtr &uno,
            const LLVMValue &value
    )
    {
        bool constant = value.constant;
        Value *result = nullptr;
        Value *val = value.value;
        bool isFpOp = val->getType()->isFloatingPointTy();
        switch (uno->op)
        {
            case TokenType::OpBitNot:
                if (isFpOp)
                    goto float_err;

                result = builder->CreateNot(val);
                break;
            case TokenType::OpLNot: // logical "not"
            case TokenType::OpNot: // c style logical not "!"
                if (isFpOp)
                    result = builder->CreateFCmpOEQ(val, ConstantFP::get(val->getType(), 0.0));
                else
                    result = builder->CreateICmpEQ(val, ConstantInt::get(val->getType(), 0));
                break;
            case TokenType::OpIncr:
                if (constant)
                    goto const_err;
                if (isFpOp)
                    result = builder->CreateFAdd(val, ConstantFP::get(val->getType(), 1.0));
                else
                    result = builder->CreateAdd(val, ConstantInt::get(val->getType(), 1));
                break;
            case TokenType::OpDecr:
                if (constant)
                    goto const_err;
                if (isFpOp)
                    result = builder->CreateFSub(val, ConstantFP::get(val->getType(), 1.0));
                else
                    result = builder->CreateSub(val, ConstantInt::get(val->getType(), 1));
                break;
            case TokenType::OpPlus: // unary plus (noop)
                result = val;
                break;
            case TokenType::OpMinus: // unary minus
                if (isFpOp)
                    result = builder->CreateFNeg(val);
                else
                {
                    //builder->CreateNSWSub()
                    result = builder->CreateSub(ConstantInt::get(val->getType(), 0), val);
                }
                break;
        }
        if (result == nullptr)
            throw shine::ShineException(filename,
                                        uno->pos,
                                        std::string("Unhandled unary operation: \"") +
                                        TokenHelper::getTokenTypeString(uno->op) + "\".");

        return LLVMValue(result, value.sign);
        const_err:
        throw ShineException(filename, uno->pos, "cannot assign to variable with constant qualifier");
        float_err:
        throw ShineException(filename,
                             uno->pos,
                             std::string("cannot call operation: \"") + TokenHelper::getTokenTypeString(uno->op) +
                             "\" with floating point values.");
    }

    LLVMValue LLVMCodegenImpl::createBinaryOperation(
            const node::BinaryOpPtr &bin,
            const LLVMValue &leftValue,
            const LLVMValue &rightValue
    )
    {
        bool constant = leftValue.constant;
        Value *result = nullptr;

        Value *lval = leftValue.value;
        Value *rval;
        bool sign = leftValue.sign;
        Type *lRealType = lval->getType()->isPointerTy() ? lval->getType()->getPointerElementType() : lval->getType();

        bool isFpOp = lRealType->isFloatingPointTy();

        if (isFpOp)
            rval = FPCast(leftValue, rightValue);
        else
            rval = IntCast(leftValue, rightValue);

        switch (bin->op)
        {
            case TokenType::OpMod:
                if (isFpOp)
                    result = builder->CreateFRem(lval, rval);
                else
                {
                    if (sign)
                        result = builder->CreateSRem(lval, rval);
                    else
                        result = builder->CreateURem(lval, rval);
                }
                break;
            case TokenType::OpGT:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_OGT, lval, rval);
                else
                    result = builder->CreateICmp(sign ? CmpInst::ICMP_SGT : CmpInst::ICMP_UGT, lval, rval);
                break;
            case TokenType::OpLT:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_OLT, lval, rval);
                else
                    result = builder->CreateICmp(sign ? CmpInst::ICMP_SLT : CmpInst::ICMP_ULT, lval, rval);
                break;
            case TokenType::OpGTE:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_OGE, lval, rval);
                else
                    result = builder->CreateICmp(sign ? CmpInst::ICMP_SGE : CmpInst::ICMP_UGE, lval, rval);
                break;
            case TokenType::OpLTE:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_OLE, lval, rval);
                else
                    result = builder->CreateICmp(sign ? CmpInst::ICMP_SLE : CmpInst::ICMP_ULE, lval, rval);
                break;
            case TokenType::OpEq:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_OEQ, lval, rval);
                else
                    result = builder->CreateICmpEQ(lval, rval);
                break;
            case TokenType::OpNEq:
                if (isFpOp)
                    result = builder->CreateFCmp(CmpInst::FCMP_ONE, lval, rval);
                else
                    result = builder->CreateICmpNE(lval, rval);
                break;
            case TokenType::OpAssign:
                if (constant)
                    goto const_err;
                result = rval;
                break;
            case TokenType::OpPlusAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpPlus:
                if (isFpOp)
                    result = builder->CreateFAdd(lval, rval);
                else
                    result = builder->CreateAdd(lval, rval);
                break;
            case TokenType::OpMinusAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpMinus:
                if (isFpOp)
                    result = builder->CreateFSub(lval, rval);
                else
                    result = builder->CreateSub(lval, rval);
                break;
            case TokenType::OpMulAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpMul:
                if (isFpOp)
                    result = builder->CreateFMul(lval, rval);
                else
                    result = builder->CreateMul(lval, rval);
                break;
            case TokenType::OpDivAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpDiv:
                if (isFpOp)
                    result = builder->CreateFDiv(lval, rval);
                else
                {
                    if (leftValue.sign)
                        result = builder->CreateSDiv(lval, rval);
                    else
                        result = builder->CreateUDiv(lval, rval);
                }
                break;
            case TokenType::OpAndAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpBitAnd:
                if (isFpOp)
                    goto float_err;
                result = builder->CreateAnd(lval, rval);
                break;
            case TokenType::OpOrAssign:
                if (constant)
                    goto const_err;
            case TokenType::OpBitOr:
                if (isFpOp)
                    goto float_err;
                result = builder->CreateOr(lval, rval);
                break;
            case TokenType::OpBitXor:
                if (isFpOp)
                    goto float_err;
                result = builder->CreateXor(lval, rval);
                break;
            case TokenType::OpBitShL:
                if (isFpOp)
                    goto float_err;
                result = builder->CreateShl(lval, rval);
                break;
            case TokenType::OpBitShR:
                if (isFpOp)
                    goto float_err;
                result = builder->CreateAShr(lval, rval);
                break;
        }
        if (result == nullptr)
            throw shine::ShineException(filename,
                                        bin->pos,
                                        std::string("Unhandled binary operation: \"") +
                                                TokenHelper::getTokenTypeString(bin->op) + "\".");

        return LLVMValue(result, sign, constant);
        const_err:
        throw ShineException(filename, bin->pos, "cannot assign to variable with constant qualifier");
        float_err:
        throw ShineException(filename,
                             bin->pos,
                             std::string("cannot call operation: \"") + TokenHelper::getTokenTypeString(bin->op) +
                             "\" with floating point values.");
    }
}
