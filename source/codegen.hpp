#pragma once

#include <string>

#include "parser.hpp"

namespace codegen {

[[nodiscard]] std::string codegen_program(const parser::Program& program);
[[nodiscard]] std::string codegen_function(const parser::Function& function);
[[nodiscard]] std::string codegen_statement(const parser::Statement& statement);
[[nodiscard]] std::string codegen_expression(
    const parser::Expression& expression);

}  // namespace codegen
