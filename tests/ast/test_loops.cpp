#include <lexer/lexer.hpp>
#include <ast/parser.hpp>


TEST_CASE("Test repeat-while loop", "[parser]") {
    shine::Lexer lexer(R"(repeat
    i += 1;
while i < 0;)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::Repeat));
    auto repeatNode = stmts.at(0)->as<shine::node::Repeat>();
    REQUIRE_FALSE(repeatNode->negate);

    auto exprNode = repeatNode->expr;
    REQUIRE(exprNode->is(shine::NodeType::BinaryOp));
    REQUIRE(exprNode->as<shine::node::BinaryOp>()->op == shine::TokenType::OpLT);

    auto binOpNode = repeatNode->block->stmts.at(0);
    REQUIRE(binOpNode->is(shine::NodeType::BinaryOp));

    auto binOp = binOpNode->as<shine::node::BinaryOp>();
    REQUIRE(binOp->op == shine::TokenType::OpPlusAssign);

    REQUIRE(binOp->left->is(shine::NodeType::Id));
    REQUIRE(binOp->left->as<shine::node::Id>()->val == "i");

    REQUIRE(binOp->right->is(shine::NodeType::Int));
    REQUIRE(binOp->right->as<shine::node::Int>()->val == 1);
}

TEST_CASE("Test while loop", "[parser]") {
    shine::Lexer lexer(R"(while i > 0
        --i;
    end)", "test.cpp");

    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::While));
    auto whileNode = stmts.at(0)->as<shine::node::While>();
    REQUIRE_FALSE(whileNode->negate);

    auto exprNode = whileNode->expr;
    REQUIRE(exprNode->is(shine::NodeType::BinaryOp));
    REQUIRE(exprNode->as<shine::node::BinaryOp>()->op == shine::TokenType::OpGT);

    auto binOpNode = whileNode->block->stmts.at(0);
    REQUIRE(binOpNode->is(shine::NodeType::UnaryOp));

    auto unoOp = binOpNode->as<shine::node::UnaryOp>();
    REQUIRE(unoOp->op == shine::TokenType::OpDecr);
    REQUIRE_FALSE(unoOp->postfix);
    REQUIRE(unoOp->expr->is(shine::NodeType::Id));
    REQUIRE(unoOp->expr->as<shine::node::Id>()->val == "i");
}

TEST_CASE("Test for loop", "[parser]") {
    shine::Lexer lexer(R"(for let i: i64 = 5; i != 0; --i;
        printf("%d\n", i);
    end)", "test.cpp");
    shine::Parser parser(&lexer);
    auto blockNode = parser.parse();
    auto stmts = blockNode->stmts;
    auto node = stmts.at(0);
    REQUIRE(node->is(shine::NodeType::For));

    auto forLoop = node->as<shine::node::For>();

    // let i: i64 = 5;
    REQUIRE(forLoop->start->is(shine::NodeType::Let));
    auto startNode = forLoop->start->as<shine::node::Let>();
    auto startArgNode = startNode->vec.at(0);
    REQUIRE(startArgNode->op == shine::TokenType::OpAssign);
    REQUIRE(startArgNode->left->is(shine::NodeType::Decl));
    auto startArgLeft = startArgNode->left->as<shine::node::Decl>();
    REQUIRE(startArgLeft->type->tname == "i64");
    REQUIRE(startArgLeft->vec.at(0)->val == "i");
    REQUIRE(startArgNode->right->is(shine::NodeType::Int));
    REQUIRE(startArgNode->right->as<shine::node::Int>()->val == 5);

    // i != 0;
    REQUIRE(forLoop->cond->is(shine::NodeType::BinaryOp));
    auto condNode = forLoop->cond->as<shine::node::BinaryOp>();
    REQUIRE(condNode->op == shine::TokenType::OpNEq);
    REQUIRE(condNode->left->is(shine::NodeType::Id));
    REQUIRE(condNode->left->as<shine::node::Id>()->val == "i");
    REQUIRE(condNode->right->is(shine::NodeType::Int));
    REQUIRE(condNode->right->as<shine::node::Int>()->val == 0);

    // --i;
    REQUIRE(forLoop->step->is(shine::NodeType::UnaryOp));
    auto unoOp = forLoop->step->as<shine::node::UnaryOp>();
    REQUIRE(unoOp->op == shine::TokenType::OpDecr);
    REQUIRE_FALSE(unoOp->postfix);
    REQUIRE(unoOp->expr->is(shine::NodeType::Id));
    REQUIRE(unoOp->expr->as<shine::node::Id>()->val == "i");

    // block till end
    REQUIRE(forLoop->block->stmts.size() == 1);
    REQUIRE(forLoop->block->stmts.at(0)->is(shine::NodeType::Call));
}
