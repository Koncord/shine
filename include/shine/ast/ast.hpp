#pragma once

#include <vector>
#include <memory>
#include <unordered_map>
#include <lexer/token.hpp>
#include <utils/position.hpp>
#include <types.hpp>
#include <utils/exception.hpp>
#include "visitor.hpp"
#include "astTypes.hpp"


namespace shine {
    namespace node {
#define SHINE_NODE_ACCEPT(Type, Spec)                               \
    void accept(Visitor &visitor, NodePtr invoker = nullptr) Spec { \
        visitor.visit(this->as<Type>(), invoker);                   \
    }

#define SHINE_NODE_ACCEPT_FINAL(Type) SHINE_NODE_ACCEPT(Type, final)
#define SHINE_NODE_ACCEPT_OVERRIDE(Type) SHINE_NODE_ACCEPT(Type, override)
#define SHINE_NODE_ACCEPT_ERR(Type) void accept(Visitor &, NodePtr) final { throw AstException(this->as<Type>()); }

        struct Node : public std::enable_shared_from_this<Node> {
            Node() = default;

            Node(NodeType type, Position pos) : nodeType(type), pos(pos) {}

            virtual ~Node() = default;

            NodeType nodeType;
            Position pos{};

            template<typename T>
            std::shared_ptr<T> as() { return std::static_pointer_cast<T>(shared_from_this()); }

            bool is(NodeType t) { return nodeType == t; }

            bool isJumpCond() { return is(NodeType::Return) || is(NodeType::Break) || is(NodeType::Continue); }

            std::string getNodeName() { return nodetype_names[(int) (nodeType)]; }
            
            virtual void accept(Visitor& visitor, NodePtr invoker = nullptr) = 0;
        };

        struct Block : Node {
            Block(Position pos) : Node(NodeType::Block, pos) {}
            SHINE_NODE_ACCEPT_FINAL(Block)

            std::vector<NodePtr> stmts;
        };

        struct Args : Node {
            Args(Position pos) : Node(NodeType::Args, pos) {}

            SHINE_NODE_ACCEPT_ERR(Args)

            std::vector<NodePtr> vec;
            std::unordered_map<std::string, NodePtr> hash;
        };

        struct Subscript : Node {
            Subscript(
                    NodePtr left,
                    NodePtr right,
                    Position pos
            ) : Node(NodeType::Subscript, pos),
                left(std::move(left)),
                right(std::move(right)) {}

            SHINE_NODE_ACCEPT_FINAL(Subscript)

            NodePtr left;
            NodePtr right;
        };

        struct Scope : Node {
            Scope(std::vector<std::string> scope, Position pos)
                    : Node(NodeType::Scope, pos),
                      scope(std::move(scope)) {}

            SHINE_NODE_ACCEPT_FINAL(Scope)

            std::vector<std::string> scope;
        };

        struct Slot : Node {
            Slot(NodePtr left, NodePtr right, Position pos)
                    : Node(NodeType::Slot, pos),
                      left(std::move(left)),
                      right(std::move(right)) {}

            SHINE_NODE_ACCEPT_FINAL(Slot)

            NodePtr left;
            NodePtr right;
        };

        struct UnaryOp : Node {
            UnaryOp(TokenType op, NodePtr expr, bool postfix, Position pos)
                    : Node(NodeType::UnaryOp, pos),
                      op(op),
                      expr(std::move(expr)),
                      postfix(postfix) {}

            SHINE_NODE_ACCEPT_FINAL(UnaryOp)

            TokenType op;
            NodePtr expr;
            bool postfix;
        };

        struct BinaryOp : Node {
            BinaryOp(TokenType op, NodePtr left, NodePtr right, Position pos)
                    : Node(NodeType::BinaryOp, pos), op(op), left(std::move(left)), right(std::move(right)),
                      let(0) {}

            SHINE_NODE_ACCEPT_FINAL(BinaryOp)

            TokenType op;
            NodePtr left;
            NodePtr right;
            int let;
        };

        template<typename Type>
        struct T_Node : Node {
            T_Node(Type val, NodeType type, Position pos) : Node(type, pos), val(std::move(val)) {}

            // SHINE_NODE_ACCEPT_FINAL(T_Node)

            Type val;
        };


        struct Int : T_Node<int64_t> {
            Int(int val, int8_t size, Position pos) : T_Node(val, NodeType::Int, pos), size(size) {}

            SHINE_NODE_ACCEPT_FINAL(Int)

            int8_t size;
        };

        struct Float : T_Node<float> {
            Float(float val, Position pos) : T_Node(val, NodeType::Float, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Float)
        };


        struct String : T_Node<std::string> {
            String(std::string val, Position pos) : T_Node(std::move(val), NodeType::String, pos) {}

            SHINE_NODE_ACCEPT_FINAL(String)
        };

        struct Boolean : T_Node<bool> {
            Boolean(bool val, Position pos) : T_Node(val, NodeType::Boolean, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Boolean)
        };

        struct Id : T_Node<std::string> {
            Id(std::string val, Position pos) : T_Node(std::move(val), NodeType::Id, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Id)
        };

        struct Type : Node {
            Type(std::string tname, int ptrLevel, Position pos) : Node(NodeType::Type, pos),
                                                                  tname(std::move(tname)),
                                                                  ptrLevel(ptrLevel),
                                                                  arrSize(0),
                                                                  isArray(false),
                                                                  isFunc(false) {}

            SHINE_NODE_ACCEPT_FINAL(Type)

            std::string tname;
            int ptrLevel;
            uint32_t arrSize;
            bool isArray;
            bool isFunc;
            std::vector<node::NodePtr> funParams;
        };

        struct VaArg : Node {
            VaArg(Position pos) : Node(NodeType::VaArg, pos) {};

            SHINE_NODE_ACCEPT_FINAL(VaArg)
        };

        struct Decl : Node {
            Decl(std::vector<IdPtr> vec, TypePtr type, Position pos)
                    : Node(NodeType::Decl, pos),
                      vec(std::move(vec)),
                      type(std::move(type)) {}

            SHINE_NODE_ACCEPT_FINAL(Decl)

            std::vector<IdPtr> vec;
            TypePtr type;
        };

        struct Variable : Node {
            Variable(std::vector<BinaryOpPtr> vec, NodeType type, Position pos)
                    : Node(type, pos),
                      vec(std::move(vec)) {}

            // void accept(Visitor &visitor) override { visitor.visit(this->as<Variable>()); }

            std::vector<BinaryOpPtr> vec;
        };

        struct Let : Variable {
            Let(std::vector<BinaryOpPtr> vec, Position pos) : Variable(std::move(vec), NodeType::Let, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Let)
        };

        struct Const : Variable {
            Const(std::vector<BinaryOpPtr> vec, Position pos) : Variable(std::move(vec), NodeType::Const, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Const)
        };

        struct Array : Node {
            Array(Position pos) : Node(NodeType::Array, pos) {}

            Array(Position pos, std::vector<NodePtr> vals) : Node(NodeType::Array, pos), vals(std::move(vals)) {}

            SHINE_NODE_ACCEPT_FINAL(Array)

            std::vector<NodePtr> vals;
        };

        struct HashPair : Node {
            HashPair(Position pos) : Node(NodeType::HashPair, pos), key(nullptr), val(nullptr) {}

            SHINE_NODE_ACCEPT_FINAL(HashPair)

            NodePtr key;
            NodePtr val;
        };

        struct Hash : Node {
            Hash(Position pos) : Node(NodeType::Hash, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Hash)

            std::vector<HashPairPtr> pairs;
        };

        struct Call : Node {
            Call(NodePtr expr, Position pos)
                    : Node(NodeType::Call, pos),
                      expr(std::move(expr)) {
                args = std::make_shared<Args>(pos);
            }

            SHINE_NODE_ACCEPT_FINAL(Call)

            NodePtr expr;
            ArgsPtr args;
        };

        struct Return : Node {
            Return(NodePtr expr, Position pos)
                    : Node(NodeType::Return, pos),
                      expr(std::move(expr)) {}

            SHINE_NODE_ACCEPT_FINAL(Return)

            NodePtr expr;
        };

        struct Proto : Node {
            Proto(
                    NodeType typeNode,
                    bool isPublic,
                    std::vector<std::string> _namespace,
                    std::string name,
                    TypePtr type,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Node(typeNode, pos),
                _namespace(std::move(_namespace)),
                name(std::move(name)),
                isPublic(isPublic),
                type(std::move(type)),
                params(std::move(params)) {}

            SHINE_NODE_ACCEPT_OVERRIDE(Proto)

            bool isPublic;
            std::vector<std::string> _namespace;
            std::string name;
            TypePtr type = nullptr;
            std::vector<NodePtr> params;
        };

        struct Function : Proto {
            Function(
                    std::vector<std::string> _namespace,
                    std::string name,
                    bool isPublic,
                    TypePtr type,
                    BlockPtr block,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Proto(NodeType::Function,
                      isPublic,
                      std::move(_namespace),
                      std::move(name),
                      std::move(type),
                      std::move(params),
                      pos),
                block(std::move(block)) {}

            Function(Proto &proto, BlockPtr block) : Proto(proto), block(std::move(block)) {}

            SHINE_NODE_ACCEPT_FINAL(Function)

            BlockPtr block;
        };

        /*struct Extern : Proto {
            Extern(
                    std::vector<std::string> _namespace,
                    std::string name,
                    bool isPublic,
                    TypePtr type,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Proto(NodeType::Extern,
                      isPublic,
                      std::move(_namespace),
                      std::move(name),
                      std::move(type),
                      std::move(params),
                      pos) {}

            SHINE_NODE_ACCEPT_FINAL(Extern)
        };*/

        struct Struct : Node {
            Struct(std::string name, Position pos) : Node(NodeType::Struct, pos), name(std::move(name)) {}

            SHINE_NODE_ACCEPT_FINAL(Struct)

            std::string name;
            std::vector<DeclPtr> fields;
        };

        struct Module : Node {
            Module(std::string name, std::vector<NodePtr> funcs, Position pos) : Node(NodeType::Module, pos),
                                                                                 name(std::move(name)),
                                                                                 funcs(std::move(funcs)) {}

            SHINE_NODE_ACCEPT_FINAL(Module)

            std::string name;
            std::vector<NodePtr> funcs;
        };

        struct If : Node {
            If(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : Node(NodeType::If, pos),
                      negate(negate),
                      expr(std::move(expr)),
                      block(std::move(block)),
                      elseBlock(nullptr) {}

            SHINE_NODE_ACCEPT_FINAL(If)

            int negate;
            NodePtr expr;
            BlockPtr block;
            BlockPtr elseBlock;
            std::vector<IfPtr> elseIfs;
        };

        struct While : Node {
            While(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : Node(NodeType::While, pos),
                      negate(negate),
                      expr(std::move(expr)),
                      block(std::move(block)) {}

            SHINE_NODE_ACCEPT_OVERRIDE(While)

            int negate;
            NodePtr expr;
            BlockPtr block;
        };

        struct Repeat : While {
            Repeat(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : While(negate, std::move(expr), std::move(block), pos) { nodeType = NodeType::Repeat; }

            SHINE_NODE_ACCEPT_FINAL(Repeat)
        };

        struct For : Node {
            For(NodePtr start, NodePtr cond, NodePtr step, BlockPtr block, Position pos)
                    : Node(NodeType::For, pos),
                      start(std::move(start)),
                      cond(std::move(cond)),
                      step(std::move(step)),
                      block(std::move(block)) {}

            SHINE_NODE_ACCEPT_FINAL(For)

            NodePtr start;
            NodePtr cond;
            NodePtr step;
            BlockPtr block;
        };

        struct Continue : Node {
            Continue(Position pos) : Node(NodeType::Continue, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Continue)
        };

        struct Break : Node {
            Break(Position pos) : Node(NodeType::Break, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Break)
        };

        struct Use : Node {
            Use(Position pos) : Node(NodeType::Use, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Use)

            std::string module;
            std::string alias;
        };

        struct Case : Node {
            Case(NodePtr expr, std::vector<WhenPtr> whenStmts, BlockPtr elseBlock, Position pos)
                    : Node(NodeType::Case, pos),
                      expr(std::move(expr)),
                      elseBlock(std::move(elseBlock)),
                      whenStmts(std::move(whenStmts)) {}

            Case(Position pos) : Node(NodeType::Case, pos) {}

            SHINE_NODE_ACCEPT_FINAL(Case)

            NodePtr expr;
            BlockPtr elseBlock;
            std::vector<WhenPtr> whenStmts;
        };

        struct When : Node {
            When(std::vector<NodePtr> exprs, BlockPtr block, Position pos) : Node(NodeType::When, pos),
                                                                             exprs(std::move(exprs)),
                                                                             block(std::move(block)) {}

            SHINE_NODE_ACCEPT_FINAL(When)

            std::vector<NodePtr> exprs;
            BlockPtr block;
        };
    }
}
