#include <lexer/lexer.hpp>
#include <ast/parser.hpp>


TEST_CASE("Test if-else statement", "[parser]") {
    shine::Lexer lexer(R"(if b == 1
        printf("%f\n", b);
    else
        printf("Never Called\n");
    end
)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;

    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::If));

    auto ifNode = node->as<shine::node::If>();
    REQUIRE_FALSE(ifNode->negate);

    REQUIRE(ifNode->expr->is(shine::NodeType::BinaryOp));
    auto binOp = ifNode->expr->as<shine::node::BinaryOp>();
    REQUIRE(binOp->op == shine::TokenType::OpEq); // just make sure that the operation is valid

    REQUIRE(ifNode->block->stmts.size() == 1); // exactly 1 statement inside "if" block

    REQUIRE_FALSE(ifNode->elseIfs.size()); // since code does not contains any else-ifs, it must be false

    REQUIRE(ifNode->elseBlock != nullptr); // checking for existence else block
    REQUIRE(ifNode->elseBlock->stmts.size() == 1); // exactly 1 statement inside "else" block
}

TEST_CASE("Test unless statement", "[parser]") {
    shine::Lexer lexer(R"(unless b == 1
        printf("%f\n", b);
    end
)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;

    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::If));

    auto ifNode = node->as<shine::node::If>();
    REQUIRE(ifNode->negate);
}

TEST_CASE("Test switch", "[parser]") {
    shine::Lexer lexer(R"(
    case i
    when 1; printf("1\n");
    when 2, 3; printf("2 or 3\n");
    else
        printf("%d\n", i);
    end)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::Case));
}
