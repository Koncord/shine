#include "prettyprint.hpp"

#include <ast/ast.hpp>
#include <ast/visitor.hpp>
#include <string>
#include <cstdio>

using namespace shine;

// print function
static int (*print_func)(const char *format, ...) = printf;

void shine::SetPrettyPrintFunc(int (*func)(const char *format, ...)) { print_func = func; }

// Return the length of an "inspected" string including the null byte.

static int inspect_length(const std::string &str)
{
    int len = 0;
    for (int i = 0; str[i]; ++i)
    {
        switch (str[i])
        {
            case '\a':
            case '\b':
            case '\e':
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

static std::string inspect(const std::string &str)
{
    // Escape chars.
    static char escapes[] = {'a', 'b', 't', 'n', 'v', 'f', 'r'};

    std::string buf;
    buf.reserve(inspect_length(str));
    for (const auto &ch : str)
    {
        switch (ch)
        {
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
            case '\e':
                buf.push_back('\\');
                buf.push_back('e');
                break;
            default:
                buf.push_back(ch);
        }
    }
    return buf;
}

namespace
{
    class Printer : public Visitor
    {
        int indents = 0;

        void indent()
        {
            for (int j = 0; j < indents; ++j)
                print_func("  ");
        }

        void visit_block(const node::BlockPtr &node) override
        {
            print_func("\n");
            for (const auto &stmt : node->stmts)
            {
                indent();
                visit(stmt);
                if (!indents) print_func("\n");
            }
        }

        void visit_id(const node::IdPtr &node) override
        {
            print_func("(id %s)", node->val.c_str());
        }

        void visit_int(const node::IntPtr &node) override
        {
            print_func("(int %d)", node->val);
        }

        void visit_float(const node::FloatPtr &node) override
        {
            print_func("(float %f)", node->val);
        }

        void visit_string(const node::StringPtr &node) override
        {
            print_func("(string '%s')", inspect(node->val).c_str());
        }

        void visit_boolean(const node::BooleanPtr &node) override
        {
            print_func("(boolean '%s')", node->val ? "true" : "false");
        }

        void visit_slot(const node::SlotPtr &node) override
        {
            print_func("(slot\n");
            ++indents;
            indent();
            visit(node->left);
            print_func("\n");
            indent();
            visit(node->right);
            --indents;
            print_func(")");
        }

        void visit_call(const node::CallPtr &node) override
        {
            print_func("(call\n");
            ++indents;
            indent();
            visit(node->expr);
            if (node->args->vec.size())
            {
                print_func("\n");
                indent();
                for (auto it = node->args->vec.begin(); it != node->args->vec.end(); ++it)
                {
                    visit(*it);
                    if (it != node->args->vec.end() - 1) print_func(" ");
                }

                for (const auto &h : node->args->hash)
                {
                    print_func(" %s: ", h.first.c_str());
                    visit(h.second);
                }
            }
            --indents;
            print_func(")");
        }

        void visit_while(const node::WhilePtr &node) override
        {
            // while | until
            print_func("(%s ", node->negate ? "until" : "while");
            visit(node->expr);
            ++indents;
            print_func("\n");
            visit(node->block);
            --indents;
            print_func(")\n");
        }

        void visit_repeat(const node::RepeatPtr &node) override
        {
            print_func("(repeat");
            ++indents;
            visit(node->block);
            --indents;
            indent();
            print_func("%s ", node->negate ? "until" : "while");
            visit(node->expr);
            print_func(")\n");
        }

        void visit_for(const node::ForPtr &node) override
        {
            print_func("(for ");
            visit(node->start);
            print_func(", ");
            visit(node->cond);
            print_func(", ");
            visit(node->step);
            ++indents;
            visit(node->block);
            --indents;
            print_func(")\n");
        }

        void visit_unary_op(const node::UnaryOpPtr &node) override
        {
            print_func("(");

            if (node->postfix)
            {
                visit(node->expr);
                print_func(" %s", TokenHelper::getTokenTypeString(node->op));
            }
            else
            {
                print_func("%s ", TokenHelper::getTokenTypeString(node->op));
                visit(node->expr);
            }

            print_func(")");
        }

        void visit_binary_op(const node::BinaryOpPtr &node) override
        {
            if (node->let)
                print_func("(let %s ", TokenHelper::getTokenTypeString(node->op));
            else
                print_func("(%s ", TokenHelper::getTokenTypeString(node->op));
            visit(node->left);
            print_func(" ");
            visit(node->right);
            print_func(")");
        }

        void visit_function(const node::FunctionPtr &node) override
        {
            print_func("(function %s -> ", node->name.c_str());
            ++indents;

            if (node->type)
                visit(node->type);

            for (const auto &param : node->params)
            {
                print_func("\n");
                indent();
                visit(param);
            }

            --indents;
            print_func("\n");
            ++indents;
            visit(node->block);
            --indents;
            print_func(")");
        }

        void visit_array(const node::ArrayPtr &node) override
        {
            print_func("(array\n");
            ++indents;
            for (auto it = node->vals.begin(); it != node->vals.end(); ++it)
            {
                indent();
                visit(*it);
                if (it != node->vals.end() - 1) print_func("\n");
            }
            --indents;
            print_func(")");
        }

        void visit_hash(const node::HashPtr &node) override
        {
            print_func("(hash\n");
            ++indents;
            for (const auto &val : node->pairs)
            {
                indent();
                auto v = std::static_pointer_cast<node::HashPair>(val);
                visit(v->key);
                print_func(": ");
                visit(v->val);
                print_func("\n");
            }
            --indents;
            print_func(")");
        }

        void visit_return(const node::ReturnPtr &node) override
        {
            print_func("(return");
            if (node->expr)
            {
                ++indents;
                print_func("\n");
                indent();
                visit(node->expr);
                --indents;
            }
            print_func(")");
        }

        void visit_decl(const node::DeclPtr &node) override
        {
            print_func("(decl");
            ++indents;
            for (const auto &val : node->vec)
            {
                print_func("\n");
                indent();
                visit(val);
            }

            if (node->type)
            {
                print_func("\n");
                indent();
                print_func(": ");
                visit(node->type);
            }

            print_func(")");
            --indents;
        }

        void visit_if(const node::IfPtr &node) override
        {
            // if
            print_func("(%s ", node->negate ? "unless" : "if");
            visit(node->expr);
            ++indents;
            print_func("\n");
            visit(node->block);
            --indents;
            print_func(")");

            // else ifs
            for (const auto &val : node->elseIfs)
            {
                auto else_if = std::static_pointer_cast<node::If>(val);
                print_func("\n");
                indent();
                print_func("(else if ");
                visit(else_if->expr);
                ++indents;
                print_func("\n");
                visit(else_if->block);
                --indents;
                print_func(")");
            }

            // else
            if (node->elseBlock)
            {
                print_func("\n");
                indent();
                print_func("(else\n");
                ++indents;
                visit(node->elseBlock);
                --indents;
                print_func(")");
            }
        }

        void visit_case(const node::CasePtr &node) override
        {
            print_func("(case ");
            visit(node->expr);
            ++indents;
            for (const auto &stmt : node->whenStmts)
            {
                auto else_if = std::static_pointer_cast<node::When>(stmt);
                print_func("\n");
                indent();
                print_func("(when ");
                for (const auto &expr : stmt->exprs)
                    visit(expr);
                ++indents;
                visit(stmt->block);
                --indents;
                print_func(")");
            }

            if (node->elseBlock)
            {
                print_func("\n");
                indent();
                print_func("(else");
                ++indents;
                visit(node->elseBlock);
                --indents;
                print_func(")");
            }
            --indents;
            print_func(")");
        }

        void visit_subscript(const node::SubscriptPtr &node) override
        {
            print_func("(subscript\n");
            ++indents;
            indent();
            visit(node->left);
            print_func("\n");
            indent();
            visit(node->right);
            --indents;
            print_func(")");
        }

        void visit_type(const node::TypePtr &node) override
        {
            print_func("(type ");

            for (int i = 0; i < node->ptrLevel; ++i)
                print_func("pointer to ");

            if (!node->funParams.empty())
                print_func ("function ");

            if (node->isArray)
                print_func("array ");

            print_func("%s", node->tname.c_str());

            if (node->isArray)
            {
                print_func("[%d]", node->arrSize);
            }

            if (!node->funParams.empty())
            {
                print_func("(");
                ++indents;
                for (int i = 0; i < node->funParams.size(); ++i)
                {
                    visit(node->funParams[i]);
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

        void visit_struct(const node::StructPtr &node) override
        {
            print_func("(type %s", node->name.c_str());
            ++indents;
            for (const auto &field : node->fields)
            {
                print_func("\n");
                indent();
                visit(field);
            }
            --indents;
            print_func(")");
        }

        void varVisit(const std::shared_ptr<node::Variable> &node)
        {
            ++indents;

            for (const auto &val : node->vec)
            {
                auto bin = std::static_pointer_cast<node::BinaryOp>(val);

                print_func("\n");
                indent();
                visit(bin->left);

                if (bin->right)
                {
                    print_func("\n");
                    indent();
                    print_func(" = ");
                    visit(bin->right);
                }
            }

            print_func(")");
            --indents;
        }
        void visit_let(const node::LetPtr &node) override
        {
            print_func("(let");
            varVisit(node);
        }

        void visit_const(const node::ConstPtr &node) override
        {
            print_func("(const");
            varVisit(node);
        }

        void visit_use(const node::UsePtr &node) override
        {
            print_func("(use \'%s\'", node->module.c_str());
            if (!node->alias.empty())
                print_func(" as %s", node->alias.c_str());
            print_func(")");
        }

        void visit_extern(const node::ExternPtr &node) override
        {
            print_func("(extern %s -> ", node->name.c_str());
            ++indents;

            if (node->type)
                visit(node->type);

            for (const auto &param : node->params)
            {
                print_func("\n");
                indent();
                visit(param);
            }

            --indents;
            print_func("\n");
            print_func(")");
        }

        void visit_continue(const node::ContinuePtr &node) override
        {
            print_func("(continue)");
        }

        void visit_break(const node::BreakPtr &node) override
        {
            print_func("(break)");
        }
        void visit_vaarg(const node::VaArgPtr &node) override
        {
            print_func("(vaarg)");
        }

    public:
        explicit Printer(const node::NodePtr &node) { visit(node); };
    };
}

// Pretty-print the given `node` to stdout.
void shine::PrettyPrint(const node::NodePtr &node)
{
    (Printer(node));

    print_func("\n");
}
