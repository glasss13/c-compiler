#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

#include "parser.hpp"

static constexpr int align_size = 16;

class Scope : public std::enable_shared_from_this<Scope> {
    struct Private {
        explicit Private() = default;
    };

public:
    Scope(std::shared_ptr<const Scope> parent,
          std::unordered_map<std::string, int> vars, int offset, Private /**/)
        : m_parent(std::move(parent)),
          m_variables(std::move(vars)),
          m_offset(offset) {}

    static std::shared_ptr<const Scope> global_scope() {
        static auto x = std::make_shared<Scope>(
            nullptr, std::unordered_map<std::string, int>{}, -align_size,
            Private{});
        return x;
    }

    std::shared_ptr<const Scope> create_child_scope() const {
        return std::make_shared<const Scope>(
            shared_from_this(), std::unordered_map<std::string, int>{},
            m_offset, Private{});
    }

    std::shared_ptr<const Scope> add_var(std::string name) const {
        auto new_vars = m_variables;
        new_vars.emplace(name, m_offset);

        return std::make_shared<const Scope>(m_parent, std::move(new_vars),
                                             m_offset - align_size, Private{});
    }

    std::optional<int> lookup(const std::string& name) const {
        auto it = m_variables.find(name);
        if (it != m_variables.end()) {
            return it->second;
        }
        if (m_parent) {
            return m_parent->lookup(name);
        }
        return std::nullopt;
    }

    std::shared_ptr<const Scope> parent() const { return m_parent; }
    int base_offset() const { return m_offset; }

private:
    std::shared_ptr<const Scope> m_parent;
    std::unordered_map<std::string, int> m_variables;
    int m_offset;
};

namespace codegen {

class CodeGenerator {
public:
    CodeGenerator() = default;
    [[nodiscard]] CodeGenerator(const CodeGenerator&) = default;
    CodeGenerator(CodeGenerator&&) = delete;
    CodeGenerator& operator=(const CodeGenerator&) = default;
    CodeGenerator& operator=(CodeGenerator&&) = delete;
    virtual ~CodeGenerator() = default;

    [[nodiscard]] virtual std::string codegen_compound_op(
        const parser::CompoundAssignmentExpression& expr) = 0;
    [[nodiscard]] virtual std::string codegen_binary_op(
        const parser::BinaryOpExpression& expr) = 0;
    [[nodiscard]] virtual std::string codegen_program(
        const parser::Program& program) = 0;
    [[nodiscard]] virtual std::string codegen_function(
        const parser::Function& function) = 0;
    [[nodiscard]] virtual std::string codegen_statement(
        const parser::Statement& statement) = 0;
    [[nodiscard]] virtual std::string codegen_expression(
        const parser::Expression& expr) = 0;
    [[nodiscard]] virtual std::string codegen_unary_op(
        const parser::UnaryOpExpression& unary_op) = 0;

    int get_label_idx() { return m_label_idx++; }
    void increment_label_idx() { ++m_label_idx; }

private:
    int m_label_idx{1};
};

class AArch64Generator : public CodeGenerator {
public:
    AArch64Generator() = default;

    [[nodiscard]] std::string codegen_compound_op(
        const parser::CompoundAssignmentExpression& expr) override;
    [[nodiscard]] std::string codegen_binary_op(
        const parser::BinaryOpExpression& expr) override;
    [[nodiscard]] std::string codegen_program(
        const parser::Program& program) override;
    [[nodiscard]] std::string codegen_function(
        const parser::Function& function) override;
    [[nodiscard]] std::string codegen_statement(
        const parser::Statement& statement) override;
    [[nodiscard]] std::string codegen_expression(
        const parser::Expression& expr) override;
    [[nodiscard]] std::string codegen_unary_op(
        const parser::UnaryOpExpression& unary_op) override;

private:
    std::shared_ptr<const Scope> m_scope{Scope::global_scope()};

    void enter_scope(std::shared_ptr<const Scope> scope) {
        m_scope = std::move(scope);
    }
    void exit_scope() { m_scope = m_scope->parent(); }
};

}  // namespace codegen
