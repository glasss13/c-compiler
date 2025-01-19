#pragma once

#include <fmt/core.h>

#include <charconv>
#include <exception>
#include <expected>
#include <memory>
#include <string>
#include <vector>

#include "lexer.hpp"

namespace parser {

enum class StatmentType { Return };
enum class FactorType { ParenGroup, UnaryOp, Constant };
enum class UnaryOpType {
    LogicalNot,
    BitwiseNot,
    Negate,
};
enum class BinaryOpType {
    Add,
    Subtract,
    Multiply,
    Divide,
};

struct AstNode {
    AstNode() = default;
    AstNode(const AstNode&) = default;
    AstNode(AstNode&&) = delete;
    AstNode& operator=(const AstNode&) = default;
    AstNode& operator=(AstNode&&) = delete;
    virtual ~AstNode() = default;

    virtual std::string to_string(int indent) const = 0;
};
struct Statement : public AstNode {
    StatmentType m_statement_type;

    explicit Statement(StatmentType type) : m_statement_type(type) {}
};
struct Expression : public AstNode {};
struct Term : public Expression {};

struct Factor : public Term {
    FactorType m_factor_type;

    explicit Factor(FactorType type) : m_factor_type(type) {}
};

struct BinaryOpExpression : public Expression {
    std::unique_ptr<Expression> m_lhs;
    std::unique_ptr<Term> m_rhs;
    BinaryOpType m_op_type;

    BinaryOpExpression(std::unique_ptr<Expression> lhs,
                       std::unique_ptr<Term> rhs, BinaryOpType op_type)
        : m_lhs(std::move(lhs)), m_rhs(std::move(rhs)), m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct TermExpression : public Expression {
    std::unique_ptr<Term> m_term;

    explicit TermExpression(std::unique_ptr<Term> term)
        : m_term(std::move(term)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct BinaryOpTerm : public Term {
    std::unique_ptr<Term> m_lhs;
    std::unique_ptr<Factor> m_rhs;
    BinaryOpType m_op_type;

    BinaryOpTerm(std::unique_ptr<Term> lhs, std::unique_ptr<Factor> rhs,
                 BinaryOpType op_type)
        : m_lhs(std::move(lhs)), m_rhs(std::move(rhs)), m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};
struct FactorTerm : public Term {
    std::unique_ptr<Factor> m_factor;

    explicit FactorTerm(std::unique_ptr<Factor> factor)
        : m_factor(std::move(factor)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct IntLiteralFactor : public Factor {
    int m_literal;

    explicit IntLiteralFactor(int literal)
        : Factor(FactorType::Constant), m_literal(literal) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct UnaryOpFactor : public Factor {
    std::unique_ptr<Factor> m_factor;
    UnaryOpType m_op_type;

    UnaryOpFactor(std::unique_ptr<Factor> factor, UnaryOpType op_type)
        : Factor(FactorType::UnaryOp),
          m_factor(std::move(factor)),
          m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct ParenGroupFactor : public Factor {
    std::unique_ptr<Expression> m_expression;

    explicit ParenGroupFactor(std::unique_ptr<Expression> expression)
        : Factor(FactorType::ParenGroup), m_expression(std::move(expression)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct ReturnStatement : public Statement {
    std::unique_ptr<Expression> m_expr;

    explicit ReturnStatement(std::unique_ptr<Expression> expr)
        : Statement(StatmentType::Return), m_expr(std::move(expr)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct Function : public AstNode {
    std::string m_name;
    std::unique_ptr<Statement> m_statement;

    Function(std::string name, std::unique_ptr<Statement> statement)
        : m_name(std::move(name)), m_statement(std::move(statement)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct Program : public AstNode {
    std::unique_ptr<Function> m_function;

    explicit Program(std::unique_ptr<Function> function)
        : m_function(std::move(function)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

std::expected<std::unique_ptr<Factor>, std::string> parse_factor(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Term>, std::string> parse_term(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    std::vector<lexer::Token>::iterator& tokens);

struct ProgramV;
struct FunctionV;
struct ReturnStatementV;
struct ParenGroupFactorV;
struct BinaryOpExpressionV;
struct BinaryOpTermV;
struct ParenGroupFactorV;
struct UnaryOpFactorV;
struct IntLiteralFactorV;

template <typename T>
using P = std::unique_ptr<T>;

using StatementV = std::variant<P<ReturnStatementV>>;

using FactorV =
    std::variant<P<ParenGroupFactorV>, P<UnaryOpFactorV>, P<IntLiteralFactorV>>;
using TermV = std::variant<FactorV, P<BinaryOpTermV>>;
using ExpressionV = std::variant<TermV, P<BinaryOpExpressionV>>;
using AstNodeV =
    std::variant<P<ProgramV>, P<FunctionV>, P<ReturnStatementV>,
                 P<BinaryOpExpressionV>, P<BinaryOpTermV>, P<ParenGroupFactorV>,
                 P<UnaryOpFactorV>, P<IntLiteralFactorV>, ExpressionV, TermV,
                 FactorV, StatementV>;

struct IntLiteralFactorV {
    int m_literal;

    explicit IntLiteralFactorV(int literal) : m_literal(literal) {}
};

struct UnaryOpFactorV {
    FactorV m_factor;
    UnaryOpType m_op_type;

    UnaryOpFactorV(FactorV factor, UnaryOpType op_type)
        : m_factor(std::move(factor)), m_op_type(op_type) {}
};

struct ParenGroupFactorV {
    ExpressionV m_expression;

    explicit ParenGroupFactorV(ExpressionV expression)
        : m_expression(std::move(expression)) {}
};

struct BinaryOpTermV {
    TermV m_lhs;
    FactorV m_rhs;
    BinaryOpType m_op_type;

    BinaryOpTermV(TermV lhs, FactorV rhs, BinaryOpType op_type)
        : m_lhs(std::move(lhs)), m_rhs(std::move(rhs)), m_op_type(op_type) {}
};

struct BinaryOpExpressionV {
    ExpressionV m_lhs;
    TermV m_rhs;
    BinaryOpType m_op_type;

    BinaryOpExpressionV(ExpressionV lhs, TermV rhs, BinaryOpType op_type)
        : m_lhs(std::move(lhs)), m_rhs(std::move(rhs)), m_op_type(op_type) {}
};

struct ReturnStatementV {
    ExpressionV m_expr;

    explicit ReturnStatementV(ExpressionV expr) : m_expr(std::move(expr)) {}
};
struct FunctionV {
    std::string m_name;
    StatementV m_statement;

    FunctionV(std::string name, StatementV statement)
        : m_name(std::move(name)), m_statement(std::move(statement)) {}
};
struct ProgramV {
    P<FunctionV> m_function;

    explicit ProgramV(P<FunctionV> function)
        : m_function(std::move(function)) {}
};

std::expected<FactorV, std::string> parse_factor_v(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<TermV, std::string> parse_term_v(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<ExpressionV, std::string> parse_expression_v(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<StatementV, std::string> parse_statement_v(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<P<FunctionV>, std::string> parse_function_v(
    std::vector<lexer::Token>::iterator& tokens);

std::expected<P<ProgramV>, std::string> parse_program_v(
    std::vector<lexer::Token>::iterator& tokens);

std::string ast_to_string(AstNodeV node, int indent = 0);

}  // namespace parser
