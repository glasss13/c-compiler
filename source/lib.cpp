#include "lib.hpp"

#include <expected>
#include <string>

#include "codegen.hpp"
#include "lexer.hpp"
#include "parser.hpp"

namespace c_compiler {
std::expected<std::string, std::string> compile_code(
    const std::string& program) {
    auto lexed = lexer::lex_program(program);
    auto parsed = parser::parse_program(lexed);
    if (!parsed) {
        return std::unexpected(parsed.error());
    }
    return codegen::codegen_program(**parsed);
}
}  // namespace c_compiler
