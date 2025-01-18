#pragma once

#include <__expected/unexpected.h>
#include <fmt/core.h>

#include <charconv>
#include <expected>
#include <string>
#include <vector>

#include "lexer.hpp"

namespace parser {

enum class StatmentType { Return };
enum class ExpressionType { Constant, UnaryOp };
enum class UnaryOpType {
    LogicalNot,
    BitwiseNot,
    Negate,
};

struct AstNode {
    AstNode() = default;
    AstNode(const AstNode&) = default;
    AstNode(AstNode&&) = delete;
    AstNode& operator=(const AstNode&) = default;
    AstNode& operator=(AstNode&&) = delete;
    virtual ~AstNode() = default;

    constexpr virtual std::string to_string() const = 0;
};
struct Statement : public AstNode {
    StatmentType m_statement_type;

    explicit Statement(StatmentType type) : m_statement_type(type) {}
};
struct Expression : public AstNode {
    ExpressionType m_expression_type;

    explicit Expression(ExpressionType type) : m_expression_type(type) {}
};

struct UnaryOpExpression : public Expression {
    std::unique_ptr<Expression> m_expression;
    UnaryOpType m_op_type;

    explicit UnaryOpExpression(std::unique_ptr<Expression> expression,
                               UnaryOpType op_type)
        : Expression(ExpressionType::UnaryOp),
          m_expression(std::move(expression)),
          m_op_type(op_type) {}

    [[nodiscard]] constexpr std::string to_string() const override {
        switch (m_op_type) {
            case UnaryOpType::LogicalNot:
                return fmt::format("!{}", m_expression->to_string());
                break;
            case UnaryOpType::BitwiseNot:
                return fmt::format("~{}", m_expression->to_string());
                break;
            case UnaryOpType::Negate:
                return fmt::format("-{}", m_expression->to_string());
                break;
        }
    }
};

struct ConstantExpression : public Expression {
    int m_constant;

    explicit ConstantExpression(int constant)
        : Expression(ExpressionType::Constant), m_constant(constant) {}

    [[nodiscard]] constexpr std::string to_string() const override {
        return fmt::format("Constant({})", m_constant);
    }
};

struct ReturnStatement : public Statement {
    std::unique_ptr<Expression> m_expr;

    explicit ReturnStatement(std::unique_ptr<Expression> expr)
        : Statement(StatmentType::Return), m_expr(std::move(expr)) {}

    [[nodiscard]] constexpr std::string to_string() const override {
        return fmt::format("ReturnStatement({})", m_expr->to_string());
    }
};

struct Function : public AstNode {
    std::string m_name;
    std::unique_ptr<Statement> m_statement;

    Function(std::string name, std::unique_ptr<Statement> statement)
        : m_name(std::move(name)), m_statement(std::move(statement)) {}

    [[nodiscard]] constexpr std::string to_string() const override {
        return fmt::format("Function(name: {}, statement: {})", m_name,
                           m_statement->to_string());
    }
};

struct Program : public AstNode {
    std::unique_ptr<Function> m_function;

    explicit Program(std::unique_ptr<Function> function)
        : m_function(std::move(function)) {}

    [[nodiscard]] constexpr std::string to_string() const override {
        return fmt::format("Program({})", m_function->to_string());
    }
};

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    std::vector<lexer::Token>::iterator& tokens);

}  // namespace parser
