#include "shine.hpp"

#include <string>
#include <cstring>
#include <stdexcept>
#include <fstream>
#include <iostream>
#include <unistd.h>

#include <llvm/Support/CommandLine.h>

#include <llvm-codegen/codegen.hpp>
#include <lexer/lexer.hpp>
#include <utils/prettyprint.hpp>
#include <utils/utils.hpp>
#include <ast/parser.hpp>

static bool ast = false;
static bool tokens = false;
static bool exec = false;
static bool genIR = false;
static int optLevel = 0;
static std::string outFile;
std::vector<std::string> sources;

void eval(std::vector<char> &source, const std::string &path) {
    shine::Lexer lex(source, path.c_str());

    if (tokens) {
        while (lex.getNextToken().type != shine::TokenType::EOS && !lex.isType(shine::TokenType::Illegal)) {
            coloredPrintf(Color::Gray, "%d", lex.getLine());
            lex.inspect();
        }
        return;
    }

    shine::Parser parser(&lex);
    shine::node::NodePtr root;

    root = parser.parse();

    if (ast) {
        shine::PrettyPrint(root);
        return;
    }


    auto llvm = shine::LLVMCodegen(root, path);
    llvm.setOptLevel(optLevel);

    std::string infile(
            std::find_if(path.rbegin(), path.rend(), [](char ch) { return ch == '\\' || ch == '/'; }).base(),
            path.end()); // todo: add support for -o flag

    if (!exec) {
        std::string outfile;

        if (outFile.empty())
            outfile = std::string(
                    infile.begin(),
                    std::find_if(infile.rbegin(), infile.rend(), [](char ch) { return ch == '.'; }).base()
            );


        if (genIR) {
            std::ofstream stream(outfile + "ll");
            llvm.print(stream);
        } else
            llvm.objectFile(outfile + "o");

        return;
    }

    if (genIR) {
        llvm.print(std::cout);
        std::cout << std::endl;
    }

    if (exec)
        llvm.execute(0, nullptr);
}

void parseFiles() {
    for (const auto &path : sources) {
        std::vector<char> source = file_read(path.c_str());

        if (source.empty())
            throw std::invalid_argument("error reading: " + path + "\n\n" + std::string(strerror(errno)) + "\n\n");

        eval(source, path);
    }
}

void parseArgs(int argc, const char **argv) {
    using namespace llvm;

    cl::SetVersionPrinter([](raw_ostream &stream) {
        stream << "Shine version " << SHINE_VERSION << "\n\n";
        cl::PrintVersionMessage();
    });

    cl::OptionCategory compilerCategory("Compiler Options", "Options for controlling the compilation process.");

    cl::list<std::string> inputs(cl::Positional, cl::Required, cl::desc("<input file>"), cl::cat(compilerCategory));

    cl::opt<std::string> outFile("o", cl::desc("Out file"), cl::value_desc("string"), cl::cat(compilerCategory));

    cl::opt<bool> printAst("ast", cl::desc("Output ast to stdout"), cl::cat(compilerCategory));
    cl::alias printAst2("A", cl::desc("Alias for -ast"), cl::aliasopt(printAst), cl::cat(compilerCategory));

    cl::opt<bool> printTokens("tokens", cl::desc("Output tokens to stdout"), cl::cat(compilerCategory));
    cl::alias printTokens2("T", cl::desc("Alias for -tokens"), cl::aliasopt(printTokens), cl::cat(compilerCategory));

    cl::opt<bool> execute("execute", cl::desc("Execute program"), cl::cat(compilerCategory));
    cl::alias execute2("E", cl::desc("Alias for -execute"), cl::aliasopt(execute), cl::cat(compilerCategory));

    cl::opt<bool> genIR("asm", cl::desc("Stop after generating IR code"), cl::cat(compilerCategory));
    cl::alias genIR2("S", cl::desc("Alias for -asm"), cl::aliasopt(genIR), cl::cat(compilerCategory));

    enum OptLevel {
        O0, O1, O2, O3
    };

    cl::opt<OptLevel> oLevel(cl::desc("Optimization level:"),
                             cl::values(
                                     clEnumVal(O0, "No optimizations, enable debugging"),
                                     clEnumVal(O1, "Enable trivial optimizations"),
                                     clEnumVal(O2, "Enable default optimizations"),
                                     clEnumVal(O3, "Enable expensive optimizations")),
                             cl::cat(compilerCategory));

    cl::ParseCommandLineOptions(argc, argv);
    sources = inputs;
    tokens = printTokens.getValue();
    ast = printAst.getValue();
    exec = execute.getValue();
    ::genIR = genIR.getValue();
    optLevel = oLevel.getValue();
    ::outFile = outFile.getValue();
}

int main(int argc, const char **argv) {
    parseArgs(argc, argv);

//    try
//    {
    parseFiles();
//    }
//    catch (std::exception &e)
//    {
//        std::cerr << e.what() << std::endl;
//        return 1;
//    }

    return 0;
}
