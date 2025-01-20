#pragma once

#include <string>

#include "parser.hpp"

namespace codegen {

class CodeGenerator {
public:
    CodeGenerator() = default;
    [[nodiscard]] CodeGenerator(const CodeGenerator&) = default;
    CodeGenerator(CodeGenerator&&) = delete;
    CodeGenerator& operator=(const CodeGenerator&) = default;
    CodeGenerator& operator=(CodeGenerator&&) = delete;
    virtual ~CodeGenerator() = default;

    [[nodiscard]] virtual std::string codegen_binary_op(
        const parser::BinaryOpExpression& expr) const = 0;
    [[nodiscard]] virtual std::string codegen_program(
        const parser::Program& program) const = 0;
    [[nodiscard]] virtual std::string codegen_function(
        const parser::Function& function) const = 0;
    [[nodiscard]] virtual std::string codegen_statement(
        const parser::Statement& statement) const = 0;
    [[nodiscard]] virtual std::string codegen_expression(
        const parser::Expression& expr) const = 0;
    [[nodiscard]] virtual std::string codegen_unary_op(
        const parser::UnaryOpExpression& unary_op) const = 0;

    int get_label_idx() const { return m_label_idx++; }
    void increment_label_idx() const { ++m_label_idx; }

private:
    mutable int m_label_idx{1};
};

class AArch64Generator : public CodeGenerator {
public:
    AArch64Generator() = default;

    [[nodiscard]] std::string codegen_binary_op(
        const parser::BinaryOpExpression& expr) const override;
    [[nodiscard]] std::string codegen_program(
        const parser::Program& program) const override;
    [[nodiscard]] std::string codegen_function(
        const parser::Function& function) const override;
    [[nodiscard]] std::string codegen_statement(
        const parser::Statement& statement) const override;
    [[nodiscard]] std::string codegen_expression(
        const parser::Expression& expr) const override;
    [[nodiscard]] std::string codegen_unary_op(
        const parser::UnaryOpExpression& unary_op) const override;
};

}  // namespace codegen
