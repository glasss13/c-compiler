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
//             auto generator = codegen::AArch64Generator();
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

TEST_CASE("Compound Add", "[integration]") {
    std::string program =
        "int main() { int x = 4; int y = 5; x += y; return x; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 9);
}

TEST_CASE("Compound Sub", "[integration]") {
    std::string program =
        "int main() { int x = 5; int y = 4; x -= y; return x; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 1);
}

TEST_CASE("Compound Multiply", "[integration]") {
    std::string program =
        "int main() { int x = 5; int y = 4; x *= y; return x; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 20);
}

TEST_CASE("Compound Divide", "[integration]") {
    std::string program =
        "int main() { int x = 26; int y = 5; x /= y; return x; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 5);
}

TEST_CASE("Compound XOR", "[integration]") {
    std::string program =
        "int main() { int x = 26; int y = 5; x ^= y; return x; }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 31);
}

TEST_CASE("Comma operator", "[integration]") {
    std::string program =
        "int main() { int a = 37; int b = (a = 3, a+1); return b;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 4);
}

TEST_CASE("Pre increment evaluates to right value", "[integration]") {
    std::string program = "int main() { int a = 4; int b = ++a; return b;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 5);
}

TEST_CASE("Pre decrement decrements", "[integration]") {
    std::string program = "int main() { int a = 4; --a; return a;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 3);
}

TEST_CASE("Post increment evaluates to right value", "[integration]") {
    std::string program = "int main() { int a = 4; int b = a++; return b;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 4);
}

TEST_CASE("Post decrement decrements", "[integration]") {
    std::string program = "int main() { int a = 4; a--; return a;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 3);
}

TEST_CASE("If statement", "[integration]") {
    std::string program =
        "int main() { int x = 4; int y = 5; if (x > y) return 100; else return "
        "47;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 47);
}

TEST_CASE("Ternary expression", "[integration]") {
    std::string program =
        "int main() { int x = 4; int y = 5; return x == y ? 100 : 47;"
        "47;}";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 47);
}

TEST_CASE("If statement block", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int x = 4;
                        int y = 5;
                        if (y > x) {
                            return y;
                        } else {
                            return x;
                        }
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 5);
}

TEST_CASE("Scope shadowing", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int x = 4;
                        int y = 5;
                        {
                            int x = 100;
                            y += x;
                        }
                        return x + y;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 109);
}

TEST_CASE("Scope shadowing 2", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        {
                            int i = 0;
                        }
                        int j = 1;
                        return j;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 1);
}

TEST_CASE("Simple for loop", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int sum = 0;
                        for (int i = 0; i < 5; i++) {
                            sum += i;
                        }
                        return sum;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 10);
}

TEST_CASE("For loop with empty body", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        for (int i = 0; i < 10; i++) {}
                        return 42;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 42);
}

TEST_CASE("While loop simple", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        int i = 0;
                        while (i < 5) {
                            count += i;
                            i++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 10);
}

TEST_CASE("While loop with condition initially false", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int x = 10;
                        while (x < 5) {
                            x++;
                        }
                        return x;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 10);
}

TEST_CASE("Do-while loop simple", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        int i = 0;
                        do {
                            count += i;
                            i++;
                        } while (i < 5);
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 10);
}

TEST_CASE("Do-while loop executes at least once", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int x = 10;
                        do {
                            x++;
                        } while (x < 5);
                        return x;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 11);
}

TEST_CASE("Nested for loops", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int sum = 0;
                        for (int i = 0; i < 3; i++) {
                            for (int j = 0; j < 3; j++) {
                                sum += 1;
                            }
                        }
                        return sum;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 9);
}

TEST_CASE("Nested while loops", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        int i = 0;
                        while (i < 3) {
                            int j = 0;
                            while (j < 2) {
                                count++;
                                j++;
                            }
                            i++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 6);
}

TEST_CASE("While loop with decrementing condition", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int x = 5;
                        while (x > 0) {
                            x--;
                        }
                        return x;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 0);
}

TEST_CASE("For loop with empty expressions", "[integration]") {
    const std::string program =
        "int main() { int x = 1; for (;;) { return x + 2; } }";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 3);
}

TEST_CASE("While loop with continue", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        int i = 0;
                        while (i < 5) {
                            if (i == 2) {
                                i++;
                                continue; 
                            }
                            count++;
                            i++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 4);
}

TEST_CASE("While loop with break", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        int i = 0;
                        while (i < 5) {
                            if (i == 3) {
                                break; 
                            }
                            count++;
                            i++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 3);
}

TEST_CASE("For loop with continue", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        for (int i = 0; i < 5; i++) {
                            if (i == 2) {
                                continue; 
                            }
                            count++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 4);
}

TEST_CASE("For loop with break", "[integration]") {
    const std::string program = R"#(
                    int main() {
                        int count = 0;
                        for (int i = 0; i < 5; i++) {
                            if (i == 3) {
                                break; 
                            }
                            count++;
                        }
                        return count;
                    }
)#";
    const auto assembly = c_compiler::compile_code(program);
    REQUIRE(assembly);
    const auto status_code = compile_and_run(*assembly, true);
    REQUIRE(status_code == 3);
}
// NOLINTEND
