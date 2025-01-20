#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

#include "codegen.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

int main() {
    // std::ifstream src("bitwise_zero.c");
    //
    // const auto tokens = lexer::lex_program(src);
    //
    // for (const auto& token : tokens) {
    //     std::cout << '[' << token.m_data << ']' << '\n';
    // }
    //
    // auto ast = parser::parse_program(tokens);
    // if (!ast) {
    //     std::cout << "failed to parse: " << ast.error() << '\n';
    // }
    //
    // std::cout << "AST:\n" << ast.value()->to_string(0) << '\n';
    //
    // auto output = codegen::codegen_program(**ast);
    // std::cout << "program:\n" << output << '\n';

    std::ofstream out("out.s");

    const std::string program = "int main() { return 5==6; }";
    const auto lex = lexer::lex_program(program);
    const auto ast = parser::parse_program(lex);
    std::cout << ast->get()->to_string(0) << '\n';

    const auto generator = codegen::AArch64Generator();
    const auto asm_out = generator.codegen_program(**ast);

    std::cout << "ASM:\n" << asm_out << '\n';

    out << asm_out;

    return 0;
}
