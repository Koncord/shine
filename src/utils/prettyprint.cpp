#include "prettyprint.hpp"

#include <ast/ast.hpp>
#include <ast/visitor.hpp>
#include <string>
#include <cstdio>
#include <utils/exception.hpp>

using namespace shine;

// print function
static int (*print_func)(const char *format, ...) = printf;

void shine::SetPrettyPrintFunc(int (*func)(const char *format, ...)) { print_func = func; }

// Return the length of an "inspected" string including the null byte.

static int inspect_length(const std::string &str) {
    int len = 0;
    for (int i = 0; str[i]; ++i) {
        switch (str[i]) {
            case '\a':
            case '\b':
            case '\x1b':
            case '\f':
            case '\n':
            case '\r':
            case '\t':
            case '\v':
                len += 2;
                break;
            default:
                ++len;
        }
    }
    return len + 1;
}

// Return an "inspected" version of the string.

static std::string inspect(const std::string &str) {
    // Escape chars.
    static char escapes[] = {'a', 'b', 't', 'n', 'v', 'f', 'r'};

    std::string buf;
    buf.reserve(inspect_length(str));
    for (const auto &ch : str) {
        switch (ch) {
            case '\a':
            case '\b':
            case '\f':
            case '\n':
            case '\r':
            case '\t':
            case '\v':
                buf.push_back('\\');
                buf.push_back(escapes[ch - 7]);
                break;
            case '\x1b':
                buf.push_back('\\');
                buf.push_back('e');
                break;
            default:
                buf.push_back(ch);
        }
    }
    return buf;
}

namespace {
    class Printer : public Visitor {
        std::string filename;
        int indents = 0;

        void indent() {
            for (int j = 0; j < indents; ++j)
                print_func("  ");
        }

        void visit(const node::BlockPtr &node, const node::NodePtr &invoker) override {
            print_func("\n");
            for (const auto &stmt : node->stmts) {
                indent();
                stmt->accept(*this);
                if (!indents) print_func("\n");
            }
        }

        void visit(const node::IdPtr &node, const node::NodePtr &invoker) override {
            print_func("(id %s)", node->val.c_str());
        }

        void visit(const node::IntPtr &node, const node::NodePtr &invoker) override {
            print_func("(int %d)", node->val);
        }

        void visit(const node::FloatPtr &node, const node::NodePtr &invoker) override {
            print_func("(float %f)", node->val);
        }

        void visit(const node::StringPtr &node, const node::NodePtr &invoker) override {
            print_func("(string '%s')", inspect(node->val).c_str());
        }

        void visit(const node::BooleanPtr &node, const node::NodePtr &invoker) override {
            print_func("(boolean '%s')", node->val ? "true" : "false");
        }

        void visit(const node::SlotPtr &node, const node::NodePtr &invoker) override {
            print_func("(slot\n");
            ++indents;
            indent();
            node->left->accept(*this);
            print_func("\n");
            indent();
            node->right->accept(*this);
            --indents;
            print_func(")");
        }

        void visit(const node::CallPtr &node, const node::NodePtr &invoker) override {
            print_func("(call\n");
            ++indents;
            indent();
            node->expr->accept(*this);
            if (node->args->vec.size()) {
                print_func("\n");
                indent();
                for (auto it = node->args->vec.begin(); it != node->args->vec.end(); ++it) {
                    (*it)->accept(*this);
                    if (it != node->args->vec.end() - 1) print_func(" ");
                }

                for (const auto &h : node->args->hash) {
                    print_func(" %s: ", h.first.c_str());
                    h.second->accept(*this);
                }
            }
            --indents;
            print_func(")");
        }

        void visit(const node::WhilePtr &node, const node::NodePtr &invoker) override {
            // while | until
            print_func("(%s ", node->negate ? "until" : "while");
            node->expr->accept(*this);
            ++indents;
            print_func("\n");
            node->block->accept(*this);
            --indents;
            print_func(")\n");
        }

        void visit(const node::RepeatPtr &node, const node::NodePtr &invoker) override {
            print_func("(repeat");
            ++indents;
            node->block->accept(*this);
            --indents;
            indent();
            print_func("%s ", node->negate ? "until" : "while");
            node->expr->accept(*this);
            print_func(")\n");
        }

        void visit(const node::ForPtr &node, const node::NodePtr &invoker) override {
            print_func("(for ");
            node->start->accept(*this);
            print_func(", ");
            node->cond->accept(*this);
            print_func(", ");
            node->step->accept(*this);
            ++indents;
            node->block->accept(*this);
            --indents;
            print_func(")\n");
        }

        void visit(const node::UnaryOpPtr &node, const node::NodePtr &invoker) override {
            print_func("(");

            if (node->postfix) {
                node->expr->accept(*this);
                print_func(" %s", TokenHelper::getTokenTypeString(node->op));
            } else {
                print_func("%s ", TokenHelper::getTokenTypeString(node->op));
                node->expr->accept(*this);
            }

            print_func(")");
        }

        void visit(const node::BinaryOpPtr &node, const node::NodePtr &invoker) override {
            if (node->let)
                print_func("(let %s ", TokenHelper::getTokenTypeString(node->op));
            else
                print_func("(%s ", TokenHelper::getTokenTypeString(node->op));
            node->left->accept(*this);
            print_func(" ");
            node->right->accept(*this);
            print_func(")");
        }

        void visit(const node::FunctionPtr &node, const node::NodePtr &invoker) override {
            std::string ns;
            for (const auto &n : node->_namespace)
                ns += n + "::";
            print_func("(%sfunction %s%s -> ", node->isPublic ? "public " : "", ns.c_str(), node->name.c_str());
            ++indents;

            if (node->type)
                node->type->accept(*this);

            for (const auto &param : node->params) {
                print_func("\n");
                indent();
                param->accept(*this);
            }

            --indents;
            //print_func("\n");
            ++indents;
            node->block->accept(*this);
            --indents;
            print_func(")");
        }

        void visit(node::ProtoPtr const &node, const node::NodePtr &invoker) override {
            std::string ns;
            for (const auto &n : node->_namespace)
                ns += n + "::";
            print_func("(%sfunction %s%s -> ", node->isPublic ? "public " : "", ns.c_str(), node->name.c_str());
            ++indents;

            if (node->type)
                node->type->accept(*this);

            for (const auto &param : node->params) {
                print_func("\n");
                indent();
                param->accept(*this);
            }

            --indents;
            print_func("\n");
            print_func(")");
        }

        void visit(const node::ArrayPtr &node, const node::NodePtr &invoker) override {
            print_func("(array\n");
            ++indents;
            for (auto it = node->vals.begin(); it != node->vals.end(); ++it) {
                indent();
                (*it)->accept(*this);
                if (it != node->vals.end() - 1) print_func("\n");
            }
            --indents;
            print_func(")");
        }

        void visit(const node::HashPtr &node, const node::NodePtr &invoker) override {
            print_func("(hash\n");
            ++indents;
            for (const auto &val : node->pairs) {
                indent();
                auto v = std::static_pointer_cast<node::HashPair>(val);
                v->key->accept(*this);
                print_func(": ");
                v->val->accept(*this);
                print_func("\n");
            }
            --indents;
            print_func(")");
        }

        void visit(const node::ReturnPtr &node, const node::NodePtr &invoker) override {
            print_func("(return");
            if (node->expr) {
                ++indents;
                print_func("\n");
                indent();
                node->expr->accept(*this);
                --indents;
            }
            print_func(")");
        }

        void visit(const node::DeclPtr &node, const node::NodePtr &invoker) override {
            print_func("(decl");
            ++indents;
            for (const auto &val : node->vec) {
                print_func("\n");
                indent();
                val->accept(*this);
            }

            if (node->type) {
                print_func("\n");
                indent();
                print_func(": ");
                node->type->accept(*this);
            }

            print_func(")");
            --indents;
        }

        void visit(const node::IfPtr &node, const node::NodePtr &invoker) override {
            // if
            print_func("(%s ", node->negate ? "unless" : "if");
            node->expr->accept(*this);
            ++indents;
            print_func("\n");
            node->block->accept(*this);
            --indents;
            print_func(")");

            // else ifs
            for (const auto &val : node->elseIfs) {
                auto else_if = std::static_pointer_cast<node::If>(val);
                print_func("\n");
                indent();
                print_func("(else if ");
                else_if->expr->accept(*this);
                ++indents;
                print_func("\n");
                else_if->block->accept(*this);
                --indents;
                print_func(")");
            }

            // else
            if (node->elseBlock) {
                print_func("\n");
                indent();
                print_func("(else\n");
                ++indents;
                node->elseBlock->accept(*this);
                --indents;
                print_func(")");
            }
        }

        void visit(const node::CasePtr &node, const node::NodePtr &invoker) override {
            print_func("(case ");
            node->expr->accept(*this);
            ++indents;
            for (const auto &stmt : node->whenStmts) {
                auto else_if = std::static_pointer_cast<node::When>(stmt);
                print_func("\n");
                indent();
                print_func("(when ");
                for (const auto &expr : stmt->exprs)
                    expr->accept(*this);
                ++indents;
                stmt->block->accept(*this);
                --indents;
                print_func(")");
            }

            if (node->elseBlock) {
                print_func("\n");
                indent();
                print_func("(else");
                ++indents;
                node->elseBlock->accept(*this);
                --indents;
                print_func(")");
            }
            --indents;
            print_func(")");
        }

        void visit(const node::SubscriptPtr &node, const node::NodePtr &invoker) override {
            print_func("(subscript\n");
            ++indents;
            indent();
            node->left->accept(*this);
            print_func("\n");
            indent();
            node->right->accept(*this);
            --indents;
            print_func(")");
        }

        void visit(const node::TypePtr &node, const node::NodePtr &invoker) override {
            print_func("(type ");

            for (int i = 0; i < node->ptrLevel; ++i)
                print_func("pointer to ");

            if (!node->funParams.empty())
                print_func("function ");

            if (node->isArray)
                print_func("array ");

            print_func("%s", node->tname.c_str());

            if (node->isArray) {
                print_func("[%d]", node->arrSize);
            }

            if (!node->funParams.empty()) {
                print_func("(");
                ++indents;
                for (uint64_t i = 0; i < node->funParams.size(); ++i) {
                    node->funParams[i]->accept(*this);
                    if (i != node->funParams.size() - 1)
                        print_func(", ");
                }
                --indents;
                print_func(")");
                indent();
                print_func("\n");
            }

            print_func(")");
        }

        void visit(const node::StructPtr &node, const node::NodePtr &invoker) override {
            print_func("(type %s", node->name.c_str());
            ++indents;
            for (const auto &field : node->fields) {
                print_func("\n");
                indent();
                field->accept(*this);
            }
            --indents;
            print_func(")");
        }

        void varVisit(const std::shared_ptr<node::Variable> &node) {
            ++indents;

            for (const auto &val : node->vec) {
                auto bin = std::static_pointer_cast<node::BinaryOp>(val);

                print_func("\n");
                indent();
                bin->left->accept(*this);

                if (bin->right) {
                    print_func("\n");
                    indent();
                    print_func(" = ");
                    bin->right->accept(*this);
                }
            }

            print_func(")");
            --indents;
        }

        void visit(const node::LetPtr &node, const node::NodePtr &invoker) override {
            print_func("(let");
            varVisit(node);
        }

        void visit(const node::ConstPtr &node, const node::NodePtr &invoker) override {
            print_func("(const");
            varVisit(node);
        }

        void visit(const node::UsePtr &node, const node::NodePtr &invoker) override {
            print_func("(use \'%s\'", node->module.c_str());
            if (!node->alias.empty())
                print_func(" as %s", node->alias.c_str());
            print_func(")");
        }

        /*void visit(const node::ExternPtr &node, const node::NodePtr &invoker) override {
            std::string ns;
            for (const auto &n : node->_namespace)
                ns += n + "::";
            print_func("(%sextern %s%s -> ", node->isPublic ? "public " : "", ns.c_str(), node->name.c_str());
            ++indents;

            if (node->type)
                node->type->accept(*this);

            for (const auto &param : node->params) {
                print_func("\n");
                indent();
                param->accept(*this);
            }

            --indents;
            print_func("\n");
            print_func(")");
        }*/

        void visit(const node::ContinuePtr &, const node::NodePtr &) override {
            print_func("(continue)");
        }

        void visit(const node::BreakPtr &, const node::NodePtr &) override {
            print_func("(break)");
        }

        void visit(const node::VaArgPtr &, const node::NodePtr &) override {
            print_func("(vaarg)");
        }

        void visit(const node::ModulePtr &node, const node::NodePtr &invoker) override {
            print_func("(module %s", node->name.c_str());
            ++indents;
            for (const auto &field : node->funcs) {
                print_func("\n");
                indent();
                field->accept(*this);
            }
            --indents;
            print_func(")");
        }

        void visit(const node::ScopePtr &node, const node::NodePtr &invoker) override {
            print_func("(scoped id ");

            for (auto it = node->scope.begin(); it != node->scope.end(); ++it) {
                print_func("%s", it->c_str());
                if (std::next(it) != std::end(node->scope))
                    print_func("::");
            }

            print_func(")");
        }

        /*void visit(node::ArgsPtr const &node, const node::NodePtr &invoker) override {
            throw shine::UnhandledNode(filename, node);
        }*/

        void visit(node::WhenPtr const &node, const node::NodePtr &invoker) override {
            throw shine::UnhandledNode(filename, node);
        }

        void visit(node::HashPairPtr const &node, const node::NodePtr &invoker) override {
            throw shine::UnhandledNode(filename, node);
        }

    public:
        explicit Printer(const node::NodePtr &node) { node->accept(*this); };
    };
}

// Pretty-print the given `node` to stdout.
void shine::PrettyPrint(const node::NodePtr &node) {
    (Printer(node));

    print_func("\n");
}
