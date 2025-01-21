#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <sstream>

#include "codegen.hpp"
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
TEST_CASE("Lex basic program", "[lex][unit]") {
    std::stringstream ss(basic_program);
    const auto lexed = lexer::lex_program(ss);

    REQUIRE(lexed[0].m_token_type == lexer::TokenType::Int);
    REQUIRE(lexed[1].m_token_type == lexer::TokenType::Identifier);
    REQUIRE(lexed[1].m_data == "main");
    REQUIRE(lexed[2].m_token_type == lexer::TokenType::OpenParen);
    REQUIRE(lexed[3].m_token_type == lexer::TokenType::CloseParen);
    REQUIRE(lexed[4].m_token_type == lexer::TokenType::OpenBrace);
    REQUIRE(lexed[5].m_token_type == lexer::TokenType::Return);
    REQUIRE(lexed[6].m_token_type == lexer::TokenType::IntLiteral);
    REQUIRE(lexed[6].m_data == "2");
    REQUIRE(lexed[7].m_token_type == lexer::TokenType::Semicolon);
    REQUIRE(lexed[8].m_token_type == lexer::TokenType::CloseBrace);
}

TEST_CASE("Parse basic program", "[parse][unit]") {
    std::stringstream ss(basic_program);
    const auto lexed = lexer::lex_program(ss);

    const auto parsed = parser::parse_program(lexed);
    REQUIRE(parsed.has_value());

    REQUIRE(parsed->get()->to_string(0) ==
            R"#(Program
  Function: main
    Return
      IntLiteral: 2)#");
}

TEST_CASE("Parse basic error", "[parse][unit]") {
    std::stringstream ss(basic_parse_error);
    const auto lexed = lexer::lex_program(ss);

    const auto parsed = parser::parse_program(lexed);
    REQUIRE(!parsed);
}

// NOLINTEND
