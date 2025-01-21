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
// TEST_CASE("Compile and check return codes",
//           "[lex][parse][codegen][integration]") {
//     auto tmp_dir = std::filesystem::temp_directory_path();
//
//     for (const auto& entry :
//          std::filesystem::directory_iterator(valid_programs_path)) {
//         if (entry.is_regular_file() && entry.path().extension() == ".c") {
//             std::ifstream file(entry.path());
//             std::string file_contents{std::istreambuf_iterator<char>(file),
//                                       std::istreambuf_iterator<char>()};
//
//             std::cerr << entry.path() << '\n';
//             std::stringstream ss(file_contents);
//             const auto lexed = lexer::lex_program(ss);
//             const auto parsed_ast = parser::parse_program(lexed);
//             REQUIRE(parsed_ast);
//             const auto generator = codegen::AArch64Generator();
//             auto codegen = generator.codegen_program(**parsed_ast);
//
//             auto our_status_code = compile_and_run(codegen, true);
//             auto reference_status_code = compile_and_run(file_contents,
//             false);
//
//             REQUIRE(reference_status_code == our_status_code);
//         }
//     }
// }

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

TEST_CASE("Logical Equality(true)", "[integration]") {
    std::string program = "int main() { return 5==5;}";
    const auto lexed = lexer::lex_program(program);
    for (const auto& l : lexed) {
        std::cerr << fmt::format("[{}]\n", l.m_data);
    }
    const auto assembly = c_compiler::compile_code(program);
    if (!assembly) {
        std::cerr << assembly.error() << '\n';
    }
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 1);
}

TEST_CASE("Logical Equality(false)", "[integration]") {
    std::string program = "int main() { return 5==6;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 0);
}
TEST_CASE("Greater than(true)", "[integration]") {
    std::string program = "int main() { return 6>5;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 1);
}
TEST_CASE("Greater than(false)", "[integration]") {
    std::string program = "int main() { return 5>6;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 0);
}
TEST_CASE("Greater than and precedence(true)", "[integration]") {
    std::string program = "int main() { return 13-5>2+2;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 1);
}
TEST_CASE("Or false", "[integration]") {
    std::string program = "int main() { return 0||0; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 0);
}

TEST_CASE("Modulo", "[integration]") {
    std::string program = "int main() { return 17%5; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 2);
}

TEST_CASE("Bitwise And", "[integration]") {
    std::string program = "int main() { return 14&7; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 6);
}
TEST_CASE("Bitwise Or", "[integration]") {
    std::string program = "int main() { return 14|7; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 15);
}
TEST_CASE("Bitwise XOr", "[integration]") {
    std::string program = "int main() { return 14^3; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 13);
}
TEST_CASE("Right Shift", "[integration]") {
    std::string program = "int main() { return -5>>4; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == static_cast<uint8_t>(-1));
}
TEST_CASE("Left Shift", "[integration]") {
    std::string program = "int main() { return 5<<4; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 80);
}

TEST_CASE("Variables", "[integration]") {
    std::string program =
        "int main() { int x = 4; int y = 5; return x * y + y; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 25);
}

TEST_CASE("Main returns 0 by default", "[integration]") {
    std::string program = "int main() { int x = 4; int y = 5; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 0);
}

// NOLINTEND
