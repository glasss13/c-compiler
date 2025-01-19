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

// NOLINTBEGIN
TEST_CASE("lex and parse valid programs", "[lex][parse][integraton]") {
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

TEST_CASE("lex and parse invalid programs", "[lex][parse][integration]") {
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
