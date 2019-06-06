#pragma once
#include <types.hpp>
#include <memory>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace shine
{
    struct LLVMCtx;
    class LLVMCodegen
    {
    public:
        LLVMCodegen(shine::node::NodePtr root, std::string_view path);
        ~LLVMCodegen();

        void print(std::ostream &os);
        void objectFile(const std::string &fname);
        void execute(int argc, char *argv[]);

        void setOptLevel(int level);

    private:
        node::NodePtr root;
        std::string fpath;
        LLVMCtx *llvmctx;
        void optimize();
        int optLevel;
    };
}
