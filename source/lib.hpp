#pragma once

#include <expected>
#include <string>

namespace c_compiler {
std::expected<std::string, std::string> compile_code(
    const std::string& program);

}  // namespace c_compiler
