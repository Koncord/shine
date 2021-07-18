# pragma once

#include <memory>

#include <types.hpp>

namespace shine {

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
  n(Repeat) \
  n(Case) \
  n(When) \
  n(Module) \
  n(Scope)        \
  n(Proto)
    enum class NodeType {
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

    namespace node {
#define n(Node) struct Node; typedef std::shared_ptr<node::Node> Node##Ptr;
        NODE_LIST
#undef n
    }
}