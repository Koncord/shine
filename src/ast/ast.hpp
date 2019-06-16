#pragma once

#include <vector>
#include <memory>
#include <unordered_map>
#include <lexer/token.hpp>
#include <lexer/position.hpp>
#include <types.hpp>


namespace shine
{

#define NODE_LIST \
  n(Block) \
  /*n(ExprStatement)*/ \
  n(Return) \
  n(If) \
  n(While) \
  n(Continue) \
  n(Break) \
  n(For) \
  n(UnaryOp) \
  n(BinaryOp) \
  /*n(TernaryOp)*/ \
  n(Boolean) \
  n(Id) \
  n(Decl) \
  n(Let) \
  n(Const) \
  n(Call) \
  n(Args) \
  n(Int) \
  n(Float) \
  n(String) \
  n(Array) \
  n(HashPair) \
  n(Hash) \
  n(Function) \
  n(Type) \
  n(VaArg) \
  n(Struct) \
  n(Slot) \
  n(Subscript) \
  n(Use) \
  n(Extern) \
  n(Repeat) \
  n(Case) \
  n(When)

    enum class NodeType
    {
#define n(Node) Node,
        NODE_LIST
#undef n
    };
    

#define STRINGIFY(a) XSTRINGIFY(a)
#define XSTRINGIFY(a) #a

    const std::string nodetype_names[]
            {
#define n(Node) STRINGIFY(Node),
                    NODE_LIST
#undef n
            };

#undef STRINGIFY
#undef XSTRINGIFY


    namespace node
    {
#define n(Node) struct Node; typedef std::shared_ptr<node::Node> Node##Ptr;
        NODE_LIST
#undef n

        struct Node : public std::enable_shared_from_this<Node>
        {
            Node() = default;

            Node(NodeType type, Position pos) : nodeType(type), pos(pos) {}

            virtual ~Node() = default;

            NodeType nodeType;
            Position pos{};

            template<typename T> std::shared_ptr<T> as() { return std::static_pointer_cast<T>(shared_from_this()); }
            bool is(NodeType t) { return nodeType == t; }
            bool isJumpCond() { return is(NodeType::Return) || is(NodeType::Break) || is(NodeType::Continue); }
            std::string getNodeName() { return nodetype_names[(int) (nodeType)]; }
        };

        struct Block : Node
        {
            Block(Position pos) : Node(NodeType::Block, pos) {}

            std::vector<NodePtr> stmts;
        };

        struct Args : Node
        {
            Args(Position pos) : Node(NodeType::Args, pos) {}

            std::vector<NodePtr> vec;
            std::unordered_map<std::string, NodePtr> hash;
        };

        struct Subscript : Node
        {
            Subscript(
                    NodePtr left,
                    NodePtr right,
                    Position pos
            ) : Node(NodeType::Subscript, pos),
                left(std::move(left)),
                right(std::move(right)) {}

            NodePtr left;
            NodePtr right;
        };


        struct Slot : Node
        {
            Slot(NodePtr left, NodePtr right, Position pos)
                    : Node(NodeType::Slot, pos),
                      left(std::move(left)),
                      right(std::move(right)) {}

            NodePtr left;
            NodePtr right;
        };

        struct UnaryOp : Node
        {
            UnaryOp(TokenType op, NodePtr expr, bool postfix, Position pos)
                    : Node(NodeType::UnaryOp, pos),
                      op(op),
                      expr(std::move(expr)),
                      postfix(postfix) {}

            TokenType op;
            NodePtr expr;
            bool postfix;
        };

        struct BinaryOp : Node
        {
            BinaryOp(TokenType op, NodePtr left, NodePtr right, Position pos)
                    : Node(NodeType::BinaryOp, pos), op(op), left(std::move(left)), right(std::move(right)),
                      let(0) {}

            TokenType op;
            NodePtr left;
            NodePtr right;
            int let;
        };

        template<typename Type>
        struct T_Node : Node
        {
            T_Node(Type val, NodeType type, Position pos) : Node(type, pos), val(std::move(val)) {}

            Type val;
        };


        struct Int : T_Node<int64_t>
        {
            Int(int val, int8_t size, Position pos) : T_Node(val, NodeType::Int, pos), size(size) {}
            int8_t size;
        };

        struct Float : T_Node<float>
        {
            Float(float val, Position pos) : T_Node(val, NodeType::Float, pos) {}
        };


        struct String : T_Node<std::string>
        {
            String(std::string val, Position pos) : T_Node(std::move(val), NodeType::String, pos) {}
        };

        struct Boolean : T_Node<bool>
        {
            Boolean(bool val, Position pos) : T_Node(val, NodeType::Boolean, pos) {}
        };

        struct Id : T_Node<std::string>
        {
            Id(std::string val, Position pos) : T_Node(std::move(val), NodeType::Id, pos) {}
        };

        struct Type : Node
        {
            Type(std::string tname, int ptrLevel, Position pos) : Node(NodeType::Type, pos),
                                                                tname(std::move(tname)),
                                                                ptrLevel(ptrLevel),
                                                                arrSize(0),
                                                                isArray(false) {}

            std::string tname;
            int ptrLevel;
            uint32_t arrSize;
            bool isArray;
        };

        struct VaArg : Node
        {
            VaArg(Position pos) : Node(NodeType::VaArg, pos) {};
        };

        struct Decl : Node
        {
            Decl(std::vector<IdPtr> vec, TypePtr type, Position pos)
                    : Node(NodeType::Decl, pos),
                      vec(std::move(vec)),
                      type(std::move(type)) {}

            std::vector<IdPtr> vec;
            TypePtr type;
        };

        struct Variable : Node
        {
            Variable(std::vector<BinaryOpPtr> vec, NodeType type, Position pos)
                    : Node(type, pos),
                      vec(std::move(vec)) {}

            std::vector<BinaryOpPtr> vec;
        };

        struct Let : Variable
        {
            Let(std::vector<BinaryOpPtr> vec, Position pos) : Variable(std::move(vec), NodeType::Let, pos) {}
        };

        struct Const : Variable
        {
            Const(std::vector<BinaryOpPtr> vec, Position pos) : Variable(std::move(vec), NodeType::Const, pos) {}
        };

        struct Array : Node
        {
            Array(Position pos) : Node(NodeType::Array, pos) {}

            Array(Position pos, std::vector<NodePtr> vals) : Node(NodeType::Array, pos), vals(std::move(vals)) {}

            std::vector<NodePtr> vals;
        };

        struct HashPair : Node
        {
            HashPair(Position pos) : Node(NodeType::HashPair, pos), key(nullptr), val(nullptr) {}

            NodePtr key;
            NodePtr val;
        };

        struct Hash : Node
        {
            Hash(Position pos) : Node(NodeType::Hash, pos) {}

            std::vector<HashPairPtr> pairs;
        };

        struct Call : Node
        {
            Call(NodePtr expr, Position pos)
                    : Node(NodeType::Call, pos),
                      expr(std::move(expr))
            {
                args = std::make_shared<Args>(pos);
            }

            NodePtr expr;
            ArgsPtr args;
        };

        struct Return : Node
        {
            Return(NodePtr expr, Position pos)
                    : Node(NodeType::Return, pos),
                      expr(std::move(expr)) {}

            NodePtr expr;
        };

        struct Proto : Node
        {
            Proto(
                    NodeType typeNode,
                    std::string name,
                    TypePtr type,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Node(typeNode, pos),
                name(std::move(name)),
                type(std::move(type)),
                params(std::move(params)) {}

            std::string name;
            TypePtr type = nullptr;
            std::vector<NodePtr> params;
        };

        struct Function : Proto
        {
            Function(
                    std::string name,
                    TypePtr type,
                    BlockPtr block,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Proto(NodeType::Function, std::move(name), std::move(type), std::move(params), pos),
                block(std::move(block)) {}

            BlockPtr block;
        };

        struct Extern : Proto
        {
            Extern(
                    std::string name,
                    TypePtr type,
                    std::vector<NodePtr> params,
                    Position pos
            ) : Proto(NodeType::Extern, std::move(name), std::move(type), std::move(params), pos) {}
        };

        struct Struct : Node
        {
            Struct(std::string name, Position pos) : Node(NodeType::Struct, pos), name(std::move(name)) {}

            std::string name;
            std::vector<DeclPtr> fields;
        };

        struct If : Node
        {
            If(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : Node(NodeType::If, pos),
                      negate(negate),
                      expr(std::move(expr)),
                      block(std::move(block)),
                      elseBlock(nullptr) {}

            int negate;
            NodePtr expr;
            BlockPtr block;
            BlockPtr elseBlock;
            std::vector<IfPtr> elseIfs;
        };

        struct While : Node
        {
            While(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : Node(NodeType::While, pos),
                      negate(negate),
                      expr(std::move(expr)),
                      block(std::move(block)) {}

            int negate;
            NodePtr expr;
            BlockPtr block;
        };

        struct Repeat : While
        {
            Repeat(int negate, NodePtr expr, BlockPtr block, Position pos)
                    : While(negate, std::move(expr), std::move(block), pos) { nodeType = NodeType::Repeat; }
        };

        struct For : Node
        {
            For(NodePtr start, NodePtr cond, NodePtr step, BlockPtr block, Position pos)
                    : Node(NodeType::For, pos),
                      start(std::move(start)),
                      cond(std::move(cond)),
                      step(std::move(step)),
                      block(std::move(block)) {}

            NodePtr start;
            NodePtr cond;
            NodePtr step;
            BlockPtr block;
        };

        struct Continue : Node
        {
            Continue(Position pos) : Node(NodeType::Continue, pos) {}
        };

        struct Break : Node
        {
            Break(Position pos) : Node(NodeType::Break, pos) {}
        };

        struct Use : Node
        {
            Use(Position pos) : Node(NodeType::Use, pos) {}

            std::string module;
            std::string alias;
        };

        struct Case : Node
        {
            Case(NodePtr expr, std::vector<WhenPtr> whenStmts, BlockPtr elseBlock, Position pos)
                    : Node(NodeType::Case, pos),
                      expr(std::move(expr)),
                      elseBlock(std::move(elseBlock)),
                      whenStmts(std::move(whenStmts)) {}

            Case(Position pos): Node(NodeType::Case, pos) {}

            NodePtr expr;
            BlockPtr elseBlock;
            std::vector<WhenPtr> whenStmts;
        };

        struct When : Node
        {
            When(std::vector<NodePtr> exprs, BlockPtr block, Position pos) : Node(NodeType::When, pos),
                                                                             exprs(std::move(exprs)),
                                                                             block(std::move(block)) {}

            std::vector<NodePtr> exprs;
            BlockPtr block;
        };
    }
}
