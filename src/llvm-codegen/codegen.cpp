#include <fstream>
#include <ast/visitor.hpp>
#include <unordered_map>
#include <stack>
#include <utils/exception.hpp>
#include "codegen.hpp"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetRegistry.h"


#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include "internal/codegen.hpp"

using namespace llvm;
using namespace shine;

namespace
{
    LLVMCtx *build(const node::NodePtr &root, std::string path)
    {
        std::string infile(
                std::find_if(path.rbegin(), path.rend(), [](char ch) { return ch == '\\' || ch == '/'; }).base(),
                path.end());
        auto ctx = new LLVMCtx();
        ctx->ctx = std::make_unique<LLVMContext>();
        ctx->module = std::make_unique<Module>(infile, *ctx->ctx);

        shine::LLVMCodegenImpl c(root, ctx, path);
        return ctx;
    }
}

void LLVMCodegen::print(std::ostream &os)
{
    InitializeNativeTargetAsmPrinter();

    if (llvmctx == nullptr)
    {
        llvmctx = build(root, fpath);
        optimize();
    }

    raw_os_ostream fileStream(os);
    llvmctx->module->print(fileStream, nullptr);
}

void LLVMCodegen::objectFile(const std::string &fname)
{
    if (llvmctx == nullptr)
    {
        llvmctx = build(root, fpath);
        optimize();
    }


    /*if (auto mainFn = llvmctx->module->getFunction("main");mainFn != nullptr)
    {
        auto builder = std::make_unique<IRBuilder<>>(*llvmctx->ctx);

        std::vector<Type *> args;
        FunctionType *fnType = FunctionType::get(builder->getVoidTy(), args, false);
        auto func = Function::Create(fnType, Function::ExternalLinkage, "_start", llvmctx->module.get());

        BasicBlock *startBlock = BasicBlock::Create(*llvmctx->ctx, "", func);
        builder->SetInsertPoint(startBlock);
        builder->CreateCall(mainFn);
        builder->CreateRetVoid();
    }*/

    auto targetTriple = sys::getDefaultTargetTriple();

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string error;
    auto target = TargetRegistry::lookupTarget(targetTriple, error);

    if (!target)
        throw std::runtime_error(error);


    auto cpu = "generic";
    auto features = "";

    TargetOptions opt;
    auto rm = Optional<Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    llvmctx->module->setDataLayout(targetMachine->createDataLayout());
    llvmctx->module->setTargetTriple(targetTriple);


    std::error_code ec;
    raw_fd_ostream fileStream(fname, ec, sys::fs::F_None);;
    if (ec)
        throw std::runtime_error("Could not open file: " + ec.message());

    legacy::PassManager pass;
    auto fileType = TargetMachine::CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, fileStream, nullptr, fileType))
        throw std::runtime_error("TargetMachine can't emit a file of this type");

    pass.run(*llvmctx->module);
    fileStream.flush();
}

void *findAndRunFnInLib(const char *lib, const char *sym)
{
    if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(lib))
        return llvm::sys::DynamicLibrary::SearchForAddressOfSymbol(sym);
    return nullptr;
}


ExitOnError ExitOnErr;

void LLVMCodegen::execute(int argc, char *argv[])
{
    if (llvmctx == nullptr)
    {
        llvmctx = build(root, fpath);
        optimize();
    }

    InitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    std::string err;
    EngineBuilder eb(std::move(llvmctx->module));
    eb.setErrorStr(&err)
      .setEngineKind(EngineKind::JIT)
      .setOptLevel(CodeGenOpt::Aggressive);
    ExecutionEngine *ee = eb.create();
    auto mainFn = reinterpret_cast<int (*)(int, char **)>(ee->getFunctionAddress("main"));

    /*auto jtmb = orc::JITTargetMachineBuilder::detectHost();
    auto dl = jtmb->getDefaultDataLayoutForTarget();
    auto lljit = ExitOnErr(orc::LLJIT::Create(*jtmb, *dl));
    auto tsm = orc::ThreadSafeModule(std::move(llvmctx->module), std::move(llvmctx->ctx));
    ExitOnErr(lljit->addIRModule(std::move(tsm)));
    auto mainSym = ExitOnErr(lljit->lookup("main"));
    auto mainFn = reinterpret_cast<int (*)(int, char **)>(mainSym.getAddress());*/


    mainFn(argc, argv);
    delete ee;
    llvm_shutdown();
}

void LLVMCodegen::optimize()
{
    assert(llvmctx != nullptr);
    if (optLevel == 0) return;
    llvm::PassBuilder passBuilder;
    llvm::LoopAnalysisManager loopAnalysisManager(true); // true is just to output debug info
    llvm::FunctionAnalysisManager functionAnalysisManager(true);
    llvm::CGSCCAnalysisManager cGSCCAnalysisManager(true);
    llvm::ModuleAnalysisManager moduleAnalysisManager(true);

    passBuilder.registerModuleAnalyses(moduleAnalysisManager);
    passBuilder.registerCGSCCAnalyses(cGSCCAnalysisManager);
    passBuilder.registerFunctionAnalyses(functionAnalysisManager);
    passBuilder.registerLoopAnalyses(loopAnalysisManager);

    passBuilder.crossRegisterProxies(loopAnalysisManager,
                                     functionAnalysisManager,
                                     cGSCCAnalysisManager,
                                     moduleAnalysisManager
    );

    PassBuilder::OptimizationLevel lvl;

    switch (optLevel)
    {
        case 1:
            lvl = llvm::PassBuilder::OptimizationLevel::O1;
            break;
        case 2:
            lvl = llvm::PassBuilder::OptimizationLevel::O2;
            break;
        case 3:
            lvl = llvm::PassBuilder::OptimizationLevel::O3;
            break;
    }

    ModulePassManager modulePassManager =
            passBuilder.buildPerModuleDefaultPipeline(lvl);
    modulePassManager.run(*llvmctx->module, moduleAnalysisManager);
}

void LLVMCodegen::setOptLevel(int level)
{
    optLevel = level;
}

LLVMCodegen::LLVMCodegen(shine::node::NodePtr root, std::string_view path) : root(std::move(root)), fpath(path.data())
{
    llvmctx = nullptr;
}

LLVMCodegen::~LLVMCodegen()
{
    delete llvmctx;
}
