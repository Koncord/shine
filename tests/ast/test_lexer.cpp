#include <lexer/lexer.hpp>


TEST_CASE("Test identifier", "[lexer]") {
    shine::Lexer lex(R"(    printf)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Id);
    std::string tokenValue = std::get<std::string>(tok.value);
    REQUIRE(tokenValue == "printf");
}

TEST_CASE("Test identifier 2", "[lexer]") {
    shine::Lexer lex(R"(
ident ident2)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Id);
    REQUIRE(std::get<std::string>(tok.value) == "ident");
    tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Id);
    REQUIRE(std::get<std::string>(tok.value) == "ident2");
}


TEST_CASE("Test string", "[lexer]") {
    shine::Lexer lex(R"("my string")", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::String);
    auto tokenValue = std::get<std::string>(tok.value);
    REQUIRE(tokenValue == "my string");
}

TEST_CASE("Test number", "[lexer]") {
    shine::Lexer lex(R"(12345)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::I64);
    auto tokenValue = std::get<int64_t>(tok.value);
    REQUIRE(tokenValue == 12345);
}

TEST_CASE("Test hex number", "[lexer]") {
    shine::Lexer lex(R"(0x12345)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::I64);
    auto tokenValue = std::get<int64_t>(tok.value);
    REQUIRE(tokenValue == 0x12345);
}

TEST_CASE("Test float number", "[lexer]") {
    shine::Lexer lex(R"(123.45f)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Float);
    auto tokenValue = std::get<double>(tok.value);
    REQUIRE(tokenValue == 123.45);
}

TEST_CASE("Test double number", "[lexer]") {
    shine::Lexer lex(R"(123.45678900)", "test.cpp");
    auto tok = lex.getNextToken();
    REQUIRE(tok.type == shine::TokenType::Float);
    auto tokenValue = std::get<double>(tok.value);
    REQUIRE(tokenValue == 123.456789);
}
