#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "codegen.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

const std::filesystem::path valid_programs_path =
    "../../../test/programs/valid";

const std::filesystem::path invalid_programs_path =
    "../../../test/programs/invalid";

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

TEST_CASE("Parse basic program", "[parse]") {
    std::stringstream ss(basic_program);
    auto lexed = lexer::lex_stream(ss);

    auto it = lexed.begin();
    const auto parsed = parser::parse_program(it);
    REQUIRE(parsed.has_value());

    std::cerr << parsed->get()->to_string(0);

    REQUIRE(parsed->get()->to_string(0) ==
            R"#(Program
  Function: main
    Return
      IntLiteral: 2)#");
}

TEST_CASE("Parse basic error", "[parse]") {
    std::stringstream ss(basic_parse_error);
    auto lexed = lexer::lex_stream(ss);

    auto it = lexed.begin();
    const auto parsed = parser::parse_program(it);
    REQUIRE(!parsed);
    REQUIRE(parsed.error().contains("semicolon"));
}

TEST_CASE("lex and parse valid programs") {
    for (const auto& entry :
         std::filesystem::directory_iterator(valid_programs_path)) {
        if (entry.is_regular_file() && entry.path().extension() == ".c") {
            std::ifstream file(entry.path());

            auto lexed = lexer::lex_stream(file);
            auto it = lexed.begin();
            const auto parsed = parser::parse_program(it);
            REQUIRE(parsed);
        }
    }
}

TEST_CASE("lex and parse invalid programs") {
    for (const auto& entry :
         std::filesystem::directory_iterator(invalid_programs_path)) {
        if (entry.is_regular_file() && entry.path().extension() == ".c") {
            std::ifstream file(entry.path());

            auto lexed = lexer::lex_stream(file);
            auto it = lexed.begin();
            const auto parsed = parser::parse_program(it);
            std::cout << entry.path() << '\n';

            for (auto l : lexed) {
                std::cout << l.m_data << '\n';
            }

            REQUIRE(!parsed);
        }
    }
}

// NOLINTEND
