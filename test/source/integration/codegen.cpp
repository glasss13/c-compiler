#include "codegen.hpp"

#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>

#include "../util.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

const std::filesystem::path valid_programs_path =
    "../../../test/programs/valid";

// NOLINTBEGIN
TEST_CASE("Compile and check return codes",
          "[lex][parse][codegen][integration]") {
    auto tmp_dir = std::filesystem::temp_directory_path();

    for (const auto& entry :
         std::filesystem::directory_iterator(valid_programs_path)) {
        if (entry.is_regular_file() && entry.path().extension() == ".c") {
            std::ifstream file(entry.path());
            std::string file_contents{std::istreambuf_iterator<char>(file),
                                      std::istreambuf_iterator<char>()};

            std::stringstream ss(file_contents);
            auto lexed = lexer::lex_program(ss);
            auto it = lexed.begin();
            const auto parsed_ast = parser::parse_program(it);
            REQUIRE(parsed_ast);
            auto codegen = codegen::codegen_program(**parsed_ast);

            auto our_status_code = compile_and_run(codegen, true);
            auto reference_status_code = compile_and_run(file_contents, false);

            REQUIRE(reference_status_code == our_status_code);
        }
    }
}

TEST_CASE("Add numbers", "[integration]") {
    std::string program = "int main() { return 5+5+3;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 13);
}

TEST_CASE("Subtract numbers", "[integration]") {
    std::string program = "int main() { return 97-50;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 47);
}

TEST_CASE("Multiply numbers", "[integration]") {
    std::string program = "int main() { return 5*7;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 35);
}

TEST_CASE("Divide numbers", "[integration]") {
    std::string program = "int main() { return 49/7;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 7);
}
// NOLINTEND
