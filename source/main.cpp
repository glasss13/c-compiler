#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

int main() {
    std::ifstream src("return_2.c");

    auto tokens = lexer::lex_stream(src);

    for (const auto& token : tokens) {
        std::cout << '[' << token.m_data << ']' << '\n';
    }

    auto it = tokens.begin();
    auto ast = parser::parse_program(it);
    if (!ast) {
        std::cout << "failed to parse: " << ast.error() << '\n';
    }
    std::cout << "AST:\n" << ast.value()->to_string() << '\n';

    return 0;
}
