#include <catch2/catch_test_macros.hpp>
#include <sstream>

#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

const std::string basic_program = R"#(
int main() {
    return 2;
}
)#";

const std::string basic_parse_error = R"#(
int main() {
    return 2
}
)#";

// NOLINTBEGIN
TEST_CASE("Lex basic program", "[lex]") {
    std::stringstream ss(basic_program);
    const auto lexed = lexer::lex_stream(ss);

    REQUIRE(lexed[0].m_token_type == lexer::TokenType::int_keyword);
    REQUIRE(lexed[1].m_token_type == lexer::TokenType::identifier);
    REQUIRE(lexed[1].m_data == "main");
    REQUIRE(lexed[2].m_token_type == lexer::TokenType::open_paren);
    REQUIRE(lexed[3].m_token_type == lexer::TokenType::close_paren);
    REQUIRE(lexed[4].m_token_type == lexer::TokenType::open_brace);
    REQUIRE(lexed[5].m_token_type == lexer::TokenType::return_keyword);
    REQUIRE(lexed[6].m_token_type == lexer::TokenType::int_literal);
    REQUIRE(lexed[6].m_data == "2");
    REQUIRE(lexed[7].m_token_type == lexer::TokenType::semicolon);
    REQUIRE(lexed[8].m_token_type == lexer::TokenType::close_brace);
}

TEST_CASE("Parse basic program", "[parse]") {
    std::stringstream ss(basic_program);
    auto lexed = lexer::lex_stream(ss);

    auto it = lexed.begin();
    const auto parsed = parser::parse_program(it);
    REQUIRE(parsed.has_value());

    REQUIRE(parsed->get()->to_string() ==
            "Program(Function(name: main, statement: "
            "ReturnStatement(Constant(2))))");
}

TEST_CASE("Parse basic error", "[parse]") {
    std::stringstream ss(basic_parse_error);
    auto lexed = lexer::lex_stream(ss);

    auto it = lexed.begin();
    const auto parsed = parser::parse_program(it);
    REQUIRE(!parsed);
    REQUIRE(parsed.error().contains("semicolon"));
}

// NOLINTEND
