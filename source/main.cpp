#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

#include "codegen.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

int main() {
    // std::ifstream src("return_2.c");
    std::ifstream src("bitwise_zero.c");

    auto tokens = lexer::lex_program(src);

    for (const auto& token : tokens) {
        std::cout << '[' << token.m_data << ']' << '\n';
    }

    auto it = tokens.begin();
    auto ast = parser::parse_program_v(it);

    // auto it = tokens.begin();
    // auto ast = parser::parse_program(it);
    if (!ast) {
        std::cout << "failed to parse: " << ast.error() << '\n';
    }

    // std::cout << "AST:\n" << ast.value()->to_string(0) << '\n';
    // std::cout << "AST:\n"
    //           << parser::ast_to_string(std::move(ast.value())) << '\n';

    // auto output = codegen::codegen_program(**ast);
    // std::cout << "program:\n" << output << '\n';

    // std::ofstream out("out.s");
    // out << output;

    return 0;
}
