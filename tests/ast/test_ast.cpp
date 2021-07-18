#include <lexer/lexer.hpp>
#include <ast/parser.hpp>


TEST_CASE("Test multiple statements", "[parser]") {
    shine::Lexer lexer(R"(printf("test"); printf("test2");)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    REQUIRE(stmts.at(0)->is(shine::NodeType::Call));
    REQUIRE(stmts.at(1)->is(shine::NodeType::Call));
}

TEST_CASE("Test extern", "[parser]") {
    shine::Lexer lexer(R"(extern printf(format: *i8, ...) : void;)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::Proto));
    auto nodeExtern = node->as<shine::node::Proto>();

    REQUIRE(nodeExtern->name == "printf");
    REQUIRE(nodeExtern->type->tname == "void");
    REQUIRE_FALSE(nodeExtern->isPublic);

    REQUIRE(nodeExtern->params.size() == 2);
    REQUIRE(nodeExtern->params[0]->is(shine::NodeType::Decl));
    REQUIRE(nodeExtern->params[1]->is(shine::NodeType::VaArg));

    auto par1 = nodeExtern->params[0]->as<shine::node::Decl>();

    REQUIRE(par1->vec.size() == 1);
    REQUIRE(par1->vec[0]->val == "format");
    REQUIRE(par1->type->tname == "i8");
    REQUIRE(par1->type->ptrLevel == 1);
}
