#include <fstream>
#include <iostream>
#include <iterator>
#include <string>

#include "codegen.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

int main() {
    std::ofstream out("out.s");

    const std::string program =
        "int main() { int x = 4; int y = 5; return x * y + y;} ";
    const auto lex = lexer::lex_program(program);
    for (const auto& l : lex) {
        std::cout << l.to_string() << '\n';
    }
    const auto ast = parser::parse_program(lex);
    if (!ast) {
        std::cout << "failed to parse ast: " << ast.error() << '\n';
        return 1;
    }
    std::cout << "AST:\n";
    std::cout << ast->get()->to_string(0) << '\n';

    auto generator = codegen::AArch64Generator();
    const auto asm_out = generator.codegen_program(**ast);

    std::cout << "ASM:\n" << asm_out << '\n';

    out << asm_out;

    return 0;
}
