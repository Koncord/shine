#include "shine.hpp"
#include <lexer/lexer.hpp>
#include <utils/prettyprint.hpp>
#include <utils/utils.hpp>
#include <ast/parser.hpp>

#include <string>
#include <stdexcept>
#include <cstring>
#include <unistd.h>

#include <tclap/CmdLine.h>
#include <llvm-codegen/codegen.hpp>
#include <fstream>

static bool ast = false;
static bool tokens = false;
static bool exec = false;
static bool genIR = false;
static int optLevel = 0;
static std::string outFile;
std::vector<std::string> sources;

/*
 * Evaluate `source` with the given
 * `path` name and return status.
 */

void eval(std::vector<char> &source, const std::string &path)
{
    shine::Lexer lex(source, path.c_str());

    if (tokens)
    {
        while (lex.getNextToken().type != shine::TokenType::EOS && !lex.isType(shine::TokenType::Illegal))
        {
            coloredPrintf(Color::Gray, "%d", lex.getLine());
            lex.inspect();
        }
        return;
    }

    shine::Parser parser(&lex);
    shine::node::NodePtr root;

    root = parser.parse();

    if (ast)
    {
        shine::PrettyPrint(root);
        return;
    }


    auto llvm = shine::LLVMCodegen(root, path);
    llvm.setOptLevel(optLevel);

    std::string infile(
            std::find_if(path.rbegin(), path.rend(), [](char ch) { return ch == '\\' || ch == '/'; }).base(),
            path.end()); // todo: add support for -o flag

    if (!exec)
    {
        std::string outfile;

        if(outFile.empty())
            outfile = std::string(
                    infile.begin(),
                            std::find_if(infile.rbegin(), infile.rend(), [](char ch) { return ch == '.'; }).base()
            );


        if (genIR)
        {
            std::ofstream stream(outfile + "ll");
            llvm.print(stream);
        }
        else
            llvm.objectFile(outfile + "o");

        return;
    }

    if (genIR)
    {
        llvm.print(std::cout);
        std::cout << std::endl;
    }

    if (exec)
        llvm.execute(0, nullptr);
}

void parseFiles()
{
    for (const auto &path : sources)
    {
        std::vector<char> source = file_read(path.c_str());

        if (source.empty())
            throw std::invalid_argument("error reading: " + path + "\n\n" + std::string(strerror(errno)) + "\n\n");

        eval(source, path);
    }
}


#ifdef WIN32
#include <windows.h>
void *hConsole;
void setupConsole()
{
    hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
}
#else

void setupConsole() {}

#endif


void parseArgs(int argc, const char **argv)
{
    TCLAP::CmdLine cmdLine("", ' ', SHINE_VERSION);
    TCLAP::SwitchArg printAst("A", "ast", "Output ast to stdout", cmdLine, false);
    TCLAP::SwitchArg printTokens("T", "tokens", "Output tokens to stdout", cmdLine, false);
    TCLAP::UnlabeledMultiArg<std::string> sourceFiles("files", "Source files", true, "file", cmdLine);
    TCLAP::SwitchArg execute("E", "execute", "Execute program", cmdLine, false);
    TCLAP::SwitchArg genIR("S", "asm", "Stop after generating IR code", cmdLine, false);
    TCLAP::ValueArg<std::string> outFile("o", "out", "Out file", false, "", "file", cmdLine);
    std::vector<int> allowed;
    for(int i = 0; i < 4; ++i)
        allowed.push_back(i);
    TCLAP::ValuesConstraint<int> constraint(allowed);
    TCLAP::ValueArg<int> oLevel("O", "opt", "Optimize IR code level", false, 2, &constraint, cmdLine);

    cmdLine.parse(argc, argv);
    sources = sourceFiles.getValue();
    tokens = printTokens.getValue();
    ast = printAst.getValue();
    exec = execute.getValue();
    ::genIR = genIR.getValue();
    optLevel = oLevel.getValue();
    ::outFile = outFile.getValue();
}

int main(int argc, const char **argv)
{
    setupConsole();

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
