#include <lexer/lexer.hpp>
#include <ast/parser.hpp>


TEST_CASE("Test array", "[parser]") {
    shine::Lexer lexer(R"(let arr : i8[10] = ['a', 'b', 'c', '\0'];)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::Let));
    auto nodeVariable = node->as<shine::node::Variable>();
    REQUIRE(nodeVariable->vec.size() == 1);

    auto var1 = nodeVariable->vec[0];
    REQUIRE(var1->op == shine::TokenType::OpAssign);
    REQUIRE(var1->left->is(shine::NodeType::Decl));
    REQUIRE(var1->right->is(shine::NodeType::Array));

    auto left = var1->left->as<shine::node::Decl>();
    REQUIRE(left->type->tname == "i8");
    REQUIRE(left->type->isArray);
    REQUIRE(left->type->arrSize == 10);

    auto right = var1->right->as<shine::node::Array>();
    REQUIRE(right->vals.size() == 4);
    std::vector<char> str;
    for (auto const &val : right->vals) {
        REQUIRE(val->is(shine::NodeType::Int));
        auto elem = val->as<shine::node::Int>();
        str.push_back((char) elem->val);
    }
    REQUIRE(str == std::vector<char>{'a', 'b', 'c', '\0'});
}
