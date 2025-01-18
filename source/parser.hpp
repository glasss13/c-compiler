#pragma once

#include <__expected/unexpected.h>

#include <charconv>
#include <expected>
#include <string>
#include <vector>

#include "lexer.hpp"

namespace parser {

struct AstNode {
    AstNode() = default;
    AstNode(const AstNode&) = default;
    AstNode(AstNode&&) = delete;
    AstNode& operator=(const AstNode&) = default;
    AstNode& operator=(AstNode&&) = delete;
    virtual ~AstNode() = default;

    virtual std::string to_string() const = 0;
};
struct Statement : public AstNode {};
struct Expression : public AstNode {};

struct ConstantExpression : public Expression {
    int m_constant;

    explicit ConstantExpression(int constant) : m_constant(constant) {}

    std::string to_string() const override;
};

struct ReturnStatement : public Statement {
    std::unique_ptr<Expression> m_expr;

    explicit ReturnStatement(std::unique_ptr<Expression> expr)
        : m_expr(std::move(expr)) {}

    std::string to_string() const override;
};

struct Function : public AstNode {
    std::string m_name;
    std::unique_ptr<Statement> m_statement;

    Function(std::string name, std::unique_ptr<Statement> statement)
        : m_name(std::move(name)), m_statement(std::move(statement)) {}

    std::string to_string() const override;
};

struct Program : public AstNode {
    std::unique_ptr<Function> m_function;

    explicit Program(std::unique_ptr<Function> function)
        : m_function(std::move(function)) {}

    std::string to_string() const override;
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
