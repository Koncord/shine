#include <lexer/lexer.hpp>
#include <ast/parser.hpp>



TEST_CASE("Test single line comments", "[lexer]") {
    shine::Lexer lex(R"(
// My comment
ident
)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Id);
    auto tokenValue = std::get<std::string>(tok.value);
    REQUIRE(tokenValue == "ident");
}

TEST_CASE("Test multi line comments", "[lexer]") {
    shine::Lexer lex(R"(
/* My comment
fake_ident
another_string*/ real_ident
)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Id);
    auto tokenValue = std::get<std::string>(tok.value);
    REQUIRE(tokenValue == "real_ident");
}
