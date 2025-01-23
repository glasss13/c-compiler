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

enum class BlockItemType {
    Declaration,
    Statement,
};
enum class StatementType { Return, Expression, If };
enum class ExpressionType {
    BinaryOp,
    UnaryOp,
    IntLiteral,
    Assignment,
    CompoundAssignment,
    VariableRef,
    Ternary,
};
enum class UnaryOpType {
    LogicalNot,
    BitwiseNot,
    Negate,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
};
enum class BinaryOpType {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    LogicalAnd,
    BitwiseOr,
    LogicalOr,
    Modulo,
    BitwiseXor,
    RightShift,
    LeftShift,
    Comma,
};
enum class CompoundAssignmentType {
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivideEqual,
    ModuloEqual,
    LeftShiftEqual,
    RightShiftEqual,
    BitwiseAndEqual,
    BitwiseOrEqual,
    XorEqual,
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

struct Expression : public AstNode {
    ExpressionType m_expr_type;

    explicit Expression(ExpressionType type) : m_expr_type(type) {}
};

struct BlockItem : public AstNode {
    BlockItemType m_item_type;

    explicit BlockItem(BlockItemType item_type) : m_item_type(item_type) {}
};
struct Statement : public BlockItem {
    StatementType m_statement_type;

    explicit Statement(StatementType type)
        : BlockItem(BlockItemType::Statement), m_statement_type(type) {}
};

struct Declaration : public BlockItem {
    std::string m_var_name;
    std::unique_ptr<Expression> m_initializer;

    explicit Declaration(std::string var_name,
                         std::unique_ptr<Expression> initializer = nullptr)
        : BlockItem(BlockItemType::Declaration),
          m_var_name(std::move(var_name)),
          m_initializer(std::move(initializer)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct BinaryOpExpression : public Expression {
    std::unique_ptr<Expression> m_lhs;
    std::unique_ptr<Expression> m_rhs;
    BinaryOpType m_op_type;

    BinaryOpExpression(std::unique_ptr<Expression> lhs,
                       std::unique_ptr<Expression> rhs, BinaryOpType op_type)
        : Expression(ExpressionType::BinaryOp),
          m_lhs(std::move(lhs)),
          m_rhs(std::move(rhs)),
          m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct IntLiteralExpression : public Expression {
    int m_literal;

    explicit IntLiteralExpression(int literal)
        : Expression(ExpressionType::IntLiteral), m_literal(literal) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct UnaryOpExpression : public Expression {
    std::unique_ptr<Expression> m_expr;
    UnaryOpType m_op_type;

    UnaryOpExpression(std::unique_ptr<Expression> expr, UnaryOpType op_type)
        : Expression(ExpressionType::UnaryOp),
          m_expr(std::move(expr)),
          m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct AssignmentExpression : public Expression {
    std::string m_var_name;
    std::unique_ptr<Expression> m_expr;

    AssignmentExpression(std::string var_name, std::unique_ptr<Expression> expr)
        : Expression(ExpressionType::Assignment),
          m_var_name(std::move(var_name)),
          m_expr(std::move(expr)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct CompoundAssignmentExpression : public Expression {
    std::string m_var_name;
    std::unique_ptr<Expression> m_expr;
    CompoundAssignmentType m_op_type;

    CompoundAssignmentExpression(std::string var_name,
                                 std::unique_ptr<Expression> expr,
                                 CompoundAssignmentType op_type)
        : Expression(ExpressionType::CompoundAssignment),
          m_var_name(std::move(var_name)),
          m_expr(std::move(expr)),
          m_op_type(op_type) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct VariableRefExpression : public Expression {
    std::string m_var_name;

    explicit VariableRefExpression(std::string var_name)
        : Expression(ExpressionType::VariableRef),
          m_var_name(std::move(var_name)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct TernaryExpression : public Expression {
    std::unique_ptr<Expression> m_cond;
    std::unique_ptr<Expression> m_then;
    std::unique_ptr<Expression> m_else;

    TernaryExpression(std::unique_ptr<Expression> cond,
                      std::unique_ptr<Expression> then,
                      std::unique_ptr<Expression> else_)
        : Expression(ExpressionType::Ternary),
          m_cond(std::move(cond)),
          m_then(std::move(then)),
          m_else(std::move(else_)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct ReturnStatement : public Statement {
    std::unique_ptr<Expression> m_expr;

    explicit ReturnStatement(std::unique_ptr<Expression> expr)
        : Statement(StatementType::Return), m_expr(std::move(expr)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct ExpressionStatement : public Statement {
    std::unique_ptr<Expression> m_expr;

    explicit ExpressionStatement(std::unique_ptr<Expression> expr)
        : Statement(StatementType::Expression), m_expr(std::move(expr)) {}
    [[nodiscard]] std::string to_string(int indent) const override;
};

struct IfStatement : public Statement {
    std::unique_ptr<Expression> m_condition;
    std::unique_ptr<Statement> m_then;
    std::unique_ptr<Statement> m_else;  // optional

    IfStatement(std::unique_ptr<Expression> condition,
                std::unique_ptr<Statement> then,
                std::unique_ptr<Statement> else_ = nullptr)
        : Statement(StatementType::If),
          m_condition(std::move(condition)),
          m_then(std::move(then)),
          m_else(std::move(else_)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct Function : public AstNode {
    std::string m_name;
    std::vector<std::unique_ptr<BlockItem>> m_block_items;

    Function(std::string name,
             std::vector<std::unique_ptr<BlockItem>> block_items)
        : m_name(std::move(name)), m_block_items(std::move(block_items)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

struct Program : public AstNode {
    std::unique_ptr<Function> m_function;

    explicit Program(std::unique_ptr<Function> function)
        : m_function(std::move(function)) {}

    [[nodiscard]] std::string to_string(int indent) const override;
};

std::expected<std::unique_ptr<Expression>, std::string> parse_ternary_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<BlockItem>, std::string> parse_block_item(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Declaration>, std::string> parse_declaration(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_unary_expression(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string>
parse_postfix_expression(lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string>
parse_assignment_expression(lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_logical_or_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string>
parse_bitwise_shift_expr(lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_or_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_xor_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_and_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_logical_and_expr(
    lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string>
parse_equality_expression(lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string>
parse_relational_expression(lexer::TokenStream& token_stream);
std::expected<std::unique_ptr<Expression>, std::string> parse_factor(
    lexer::TokenStream& token_stream);

std::expected<std::unique_ptr<Expression>, std::string> parse_term(
    lexer::TokenStream& token_stream);

std::expected<std::unique_ptr<Expression>, std::string>
parse_additive_expression(lexer::TokenStream& token_stream);

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    lexer::TokenStream& token_stream);

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    lexer::TokenStream& token_stream);

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    lexer::TokenStream token_stream);

// struct ProgramV;
// struct FunctionV;
// struct ReturnStatementV;
// struct ParenGroupFactorV;
// struct BinaryOpExpressionV;
// struct ParenGroupFactorV;
// struct UnaryOpFactorV;
// struct IntLiteralFactorV;
//
// template <typename T>
// using P = std::unique_ptr<T>;
//
// using StatementV = std::variant<P<ReturnStatementV>>;
//
// using FactorV =
//     std::variant<P<ParenGroupFactorV>, P<UnaryOpFactorV>,
//     P<IntLiteralFactorV>>;
// using TermV = std::variant<FactorV, P<BinaryOpExpressionV>>;
// using ExpressionV = std::variant<TermV, P<BinaryOpExpressionV>>;
// using AstNodeV = std::variant<P<ProgramV>, P<FunctionV>,
// P<ReturnStatementV>,
//                               P<BinaryOpExpressionV>,
//                               P<ParenGroupFactorV>, P<UnaryOpFactorV>,
//                               P<IntLiteralFactorV>, ExpressionV, TermV,
//                               FactorV, StatementV>;
//
// struct IntLiteralFactorV {
//     int m_literal;
//
//     explicit IntLiteralFactorV(int literal) : m_literal(literal) {}
// };
//
// struct UnaryOpFactorV {
//     FactorV m_factor;
//     UnaryOpType m_op_type;
//
//     UnaryOpFactorV(FactorV factor, UnaryOpType op_type)
//         : m_factor(std::move(factor)), m_op_type(op_type) {}
// };
//
// struct ParenGroupFactorV {
//     ExpressionV m_expression;
//
//     explicit ParenGroupFactorV(ExpressionV expression)
//         : m_expression(std::move(expression)) {}
// };
//
// struct BinaryOpExpressionV {
//     ExpressionV m_lhs;
//     TermV m_rhs;
//     BinaryOpType m_op_type;
//
//     BinaryOpExpressionV(ExpressionV lhs, TermV rhs, BinaryOpType op_type)
//         : m_lhs(std::move(lhs)), m_rhs(std::move(rhs)),
//         m_op_type(op_type) {}
// };
//
// struct ReturnStatementV {
//     ExpressionV m_expr;
//
//     explicit ReturnStatementV(ExpressionV expr) : m_expr(std::move(expr))
//     {}
// };
// struct FunctionV {
//     std::string m_name;
//     StatementV m_statement;
//
//     FunctionV(std::string name, StatementV statement)
//         : m_name(std::move(name)), m_statement(std::move(statement)) {}
// };
// struct ProgramV {
//     P<FunctionV> m_function;
//
//     explicit ProgramV(P<FunctionV> function)
//         : m_function(std::move(function)) {}
// };
//
// std::expected<FactorV, std::string> parse_factor_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::expected<TermV, std::string> parse_term_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::expected<ExpressionV, std::string> parse_expression_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::expected<StatementV, std::string> parse_statement_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::expected<P<FunctionV>, std::string> parse_function_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::expected<P<ProgramV>, std::string> parse_program_v(
//     std::vector<lexer::Token>::iterator& tokens);
//
// std::string ast_to_string(AstNodeV node, int indent = 0);

}  // namespace parser
