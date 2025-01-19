#include "parser.hpp"

#include <__expected/unexpect.h>
#include <fmt/core.h>

#include "lexer.hpp"

namespace {

inline std::string get_indent(int level) {
    std::string out(static_cast<size_t>(level * 2), ' ');
    return out;
}
}  // namespace

namespace parser {

std::expected<std::unique_ptr<Factor>, std::string> parse_factor(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;
    auto token = *tokens;
    ++tokens;

    switch (token.m_token_type) {
        case TokenType::IntLiteral: {
            int constant{};
            std::from_chars(token.m_data.data(),
                            token.m_data.data() + token.m_data.size(),
                            constant);

            return std::make_unique<IntLiteralFactor>(constant);
        }
        case TokenType::Tilde:
        case TokenType::Bang:
        case TokenType::Dash: {
            auto child_expr = parse_factor(tokens);
            if (!child_expr) {
                return std::unexpected(child_expr.error());
            }

            const auto type = token.m_token_type == TokenType::Tilde
                                  ? UnaryOpType::BitwiseNot
                              : token.m_token_type == TokenType::Bang
                                  ? UnaryOpType::LogicalNot
                                  : UnaryOpType::Negate;

            return std::make_unique<UnaryOpFactor>(
                std::move(child_expr.value()), type);
        }
        case TokenType::OpenParen: {
            auto child_expr = parse_expression(tokens);
            if (!child_expr) {
                return std::unexpected(child_expr.error());
            }
            if (tokens->m_token_type != TokenType::CloseParen) {
                return std::unexpected(
                    "Failed to parse factor: expected closing paren");
            }
            ++tokens;
            return std::make_unique<ParenGroupFactor>(
                std::move(child_expr.value()));
        }
        default:
            return std::unexpected(fmt::format(
                "Failed to parse factor: expected integer literal or "
                "unary operator, received: {}",
                token.m_data));
    }
}

std::expected<std::unique_ptr<Term>, std::string> parse_term(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;

    auto maybe_factor = parse_factor(tokens);
    if (!maybe_factor) {
        return std::unexpected(maybe_factor.error());
    }

    std::unique_ptr<Term> factor = std::move(maybe_factor.value());

    while (tokens->m_token_type == TokenType::Asterisk ||
           tokens->m_token_type == TokenType::ForwardSlash) {
        auto op_type = tokens->m_token_type == TokenType::Asterisk
                           ? BinaryOpType::Multiply
                           : BinaryOpType::Divide;
        ++tokens;

        auto next_factor = parse_factor(tokens);
        if (!next_factor) {
            return std::unexpected(next_factor.error());
        }

        factor = std::make_unique<BinaryOpTerm>(
            std::move(factor), std::move(next_factor.value()), op_type);
    }

    return factor;
}

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;

    auto maybe_term = parse_term(tokens);
    if (!maybe_term) {
        return std::unexpected(maybe_term.error());
    }
    std::unique_ptr<Expression> term = std::move(maybe_term.value());

    while (tokens->m_token_type == TokenType::Plus ||
           tokens->m_token_type == TokenType::Dash) {
        auto op_type = tokens->m_token_type == TokenType::Plus
                           ? BinaryOpType::Add
                           : BinaryOpType::Subtract;
        ++tokens;

        auto next_term = parse_term(tokens);
        if (!next_term) {
            return std::unexpected(next_term.error());
        }
        term = std::make_unique<BinaryOpExpression>(
            std::move(term), std::move(next_term.value()), op_type);
    }

    return term;
}

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::Return) {
        return std::unexpected(
            "Failed to parse statement: missing return keyword");
    }
    ++tokens;
    auto expr = parse_expression(tokens);
    if (!expr) {
        return std::unexpected(expr.error());
    }

    if (tokens->m_token_type != lexer::TokenType::Semicolon) {
        return std::unexpected("Failed to parse statement: missing semicolon");
    }
    ++tokens;

    return std::make_unique<ReturnStatement>(std::move(expr.value()));
}

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::Int) {
        return std::unexpected(
            "Failed to parse function: malformed return type");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::Identifier) {
        return std::unexpected("Failed to parse function: malformed name");
    }
    auto name = tokens->m_data;
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::OpenParen) {
        return std::unexpected("Failed to parse function: missing open paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::CloseParen) {
        return std::unexpected(
            "Failed to parse function: missing closing paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::OpenBrace) {
        return std::unexpected("Failed to parse function: missing open brace");
    }
    ++tokens;

    auto statement = parse_statement(tokens);

    if (!statement) {
        return std::unexpected(statement.error());
    }

    if (tokens->m_token_type != lexer::TokenType::CloseBrace) {
        return std::unexpected(
            "Failed to parse function: missing closing brace");
    }
    ++tokens;

    return std::make_unique<Function>(name, std::move(statement.value()));
}

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    std::vector<lexer::Token>::iterator& tokens) {
    auto function = parse_function(tokens);
    if (!function) {
        return std::unexpected(function.error());
    }

    return std::make_unique<Program>(std::move(function.value()));
}

[[nodiscard]] std::string BinaryOpExpression::to_string(int indent) const {
    const auto op_char = [&]() {
        switch (m_op_type) {
            case BinaryOpType::Add:
                return '+';
            case BinaryOpType::Subtract:
                return '-';
            case BinaryOpType::Multiply:
                return '*';
            case BinaryOpType::Divide:
                return '/';
            default:
                std::unreachable();
        }
    }();

    return fmt::format("{}BinaryOpExpr: {}\n{}Left:\n{}\n{}Right:\n{}",
                       get_indent(indent), op_char, get_indent(indent + 1),
                       m_lhs->to_string(indent + 2), get_indent(indent + 1),
                       m_rhs->to_string(indent + 2));
}

[[nodiscard]] std::string TermExpression::to_string(int indent) const {
    return m_term->to_string(indent);
}

[[nodiscard]] std::string BinaryOpTerm::to_string(int indent) const {
    const auto op_char = [&]() {
        switch (m_op_type) {
            case BinaryOpType::Multiply:
                return '*';
            case BinaryOpType::Divide:
                return '/';
            default:
                std::unreachable();
        }
    }();

    return fmt::format("{}BinaryOpTerm: {}\n{}Left:\n{}\n{}Right:\n{}",
                       get_indent(indent), op_char, get_indent(indent + 1),
                       m_lhs->to_string(indent + 2), get_indent(indent + 1),
                       m_rhs->to_string(indent + 2));
}

[[nodiscard]] std::string FactorTerm::to_string(int indent) const {
    return m_factor->to_string(indent);
}

[[nodiscard]] std::string IntLiteralFactor::to_string(int indent) const {
    return fmt::format("{}IntLiteral: {}", get_indent(indent), m_literal);
}

[[nodiscard]] std::string UnaryOpFactor::to_string(int indent) const {
    const auto op = [&]() {
        switch (m_op_type) {
            case UnaryOpType::LogicalNot:
                return '!';
            case UnaryOpType::BitwiseNot:
                return '~';
            case UnaryOpType::Negate:
                return '-';
        }
    }();

    return fmt::format("{}UnaryOp: {}\n{}", get_indent(indent), op,
                       m_factor->to_string(indent + 1));
}

[[nodiscard]] std::string ParenGroupFactor::to_string(int indent) const {
    return fmt::format("{}ParenGroup\n{}", get_indent(indent),
                       m_expression->to_string(indent + 1));
}

[[nodiscard]] std::string ReturnStatement::to_string(int indent) const {
    return fmt::format("{}Return\n{}", get_indent(indent),
                       m_expr->to_string(indent + 1));
}

[[nodiscard]] std::string Function::to_string(int indent) const {
    return fmt::format("{}Function: {}\n{}", get_indent(indent), m_name,
                       m_statement->to_string(indent + 1));
}

[[nodiscard]] std::string Program::to_string(int indent) const {
    return fmt::format("{}Program\n{}", get_indent(indent),
                       m_function->to_string(indent + 1));
}

[[nodiscard]] std::expected<FactorV, std::string> parse_factor_v(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;
    auto token = *tokens;
    ++tokens;

    switch (token.m_token_type) {
        case TokenType::IntLiteral: {
            int constant{};
            std::from_chars(token.m_data.data(),
                            token.m_data.data() + token.m_data.size(),
                            constant);

            return std::make_unique<IntLiteralFactorV>(constant);
        }
        case TokenType::Tilde:
        case TokenType::Bang:
        case TokenType::Dash: {
            auto child_expr = parse_factor_v(tokens);
            if (!child_expr) {
                return std::unexpected(child_expr.error());
            }

            const auto type = token.m_token_type == TokenType::Tilde
                                  ? UnaryOpType::BitwiseNot
                              : token.m_token_type == TokenType::Bang
                                  ? UnaryOpType::LogicalNot
                                  : UnaryOpType::Negate;

            return std::make_unique<UnaryOpFactorV>(
                std::move(child_expr.value()), type);
        }
        case TokenType::OpenParen: {
            auto child_expr = parse_expression_v(tokens);
            if (!child_expr) {
                return std::unexpected(child_expr.error());
            }
            if (tokens->m_token_type != TokenType::CloseParen) {
                return std::unexpected(
                    "Failed to parse factor: expected closing paren");
            }
            ++tokens;
            return std::make_unique<ParenGroupFactorV>(
                std::move(child_expr.value()));
        }
        default:
            return std::unexpected(fmt::format(
                "Failed to parse factor: expected integer literal or "
                "unary operator, received: {}",
                token.m_data));
    }
}

[[nodiscard]] std::expected<TermV, std::string> parse_term_v(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;

    auto maybe_factor = parse_factor_v(tokens);
    if (!maybe_factor) {
        return std::unexpected(maybe_factor.error());
    }

    TermV factor = std::move(maybe_factor.value());

    while (tokens->m_token_type == TokenType::Asterisk ||
           tokens->m_token_type == TokenType::ForwardSlash) {
        auto op_type = tokens->m_token_type == TokenType::Asterisk
                           ? BinaryOpType::Multiply
                           : BinaryOpType::Divide;
        ++tokens;

        auto next_factor = parse_factor_v(tokens);
        if (!next_factor) {
            return std::unexpected(next_factor.error());
        }

        factor = std::make_unique<BinaryOpTermV>(
            std::move(factor), std::move(next_factor.value()), op_type);
    }

    return factor;
}

[[nodiscard]] std::expected<ExpressionV, std::string> parse_expression_v(
    std::vector<lexer::Token>::iterator& tokens) {
    using lexer::TokenType;

    auto maybe_term = parse_term_v(tokens);
    if (!maybe_term) {
        return std::unexpected(maybe_term.error());
    }
    ExpressionV term = std::move(maybe_term.value());

    while (tokens->m_token_type == TokenType::Plus ||
           tokens->m_token_type == TokenType::Dash) {
        auto op_type = tokens->m_token_type == TokenType::Plus
                           ? BinaryOpType::Add
                           : BinaryOpType::Subtract;
        ++tokens;

        auto next_term = parse_term_v(tokens);
        if (!next_term) {
            return std::unexpected(next_term.error());
        }
        term = std::make_unique<BinaryOpExpressionV>(
            std::move(term), std::move(next_term.value()), op_type);
    }

    return term;
}

[[nodiscard]] std::expected<StatementV, std::string> parse_statement_v(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::Return) {
        return std::unexpected(
            "Failed to parse statement: missing return keyword");
    }
    ++tokens;
    auto expr = parse_expression_v(tokens);
    if (!expr) {
        return std::unexpected(expr.error());
    }

    if (tokens->m_token_type != lexer::TokenType::Semicolon) {
        return std::unexpected("Failed to parse statement: missing semicolon");
    }
    ++tokens;

    return std::make_unique<ReturnStatementV>(std::move(expr.value()));
}

[[nodiscard]] std::expected<P<FunctionV>, std::string> parse_function_v(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::Int) {
        return std::unexpected(
            "Failed to parse function: malformed return type");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::Identifier) {
        return std::unexpected("Failed to parse function: malformed name");
    }
    auto name = tokens->m_data;
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::OpenParen) {
        return std::unexpected("Failed to parse function: missing open paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::CloseParen) {
        return std::unexpected(
            "Failed to parse function: missing closing paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::OpenBrace) {
        return std::unexpected("Failed to parse function: missing open brace");
    }
    ++tokens;

    auto statement = parse_statement_v(tokens);

    if (!statement) {
        return std::unexpected(statement.error());
    }

    if (tokens->m_token_type != lexer::TokenType::CloseBrace) {
        return std::unexpected(
            "Failed to parse function: missing closing brace");
    }
    ++tokens;

    return std::make_unique<FunctionV>(name, std::move(statement.value()));
}

[[nodiscard]] std::expected<P<ProgramV>, std::string> parse_program_v(
    std::vector<lexer::Token>::iterator& tokens) {
    auto function = parse_function_v(tokens);
    if (!function) {
        return std::unexpected(function.error());
    }

    return std::make_unique<ProgramV>(std::move(function.value()));
}

std::string ast_to_string(AstNodeV node, int indent) {
    struct Visit {
        int indent;
        std::string operator()(P<ProgramV>& n) const {
            return fmt::format(
                "{}Program\n{}", get_indent(indent),
                ast_to_string(std::move(n->m_function), indent + 1));
        }
        std::string operator()(P<FunctionV>& n) const {
            return fmt::format(
                "{}Function: {}\n{}", get_indent(indent), n->m_name,
                ast_to_string(std::move(n->m_statement), indent + 1));
        }
        std::string operator()(StatementV& n) const {
            return std::visit(
                [this](auto& p) { return ast_to_string(std::move(p), indent); },
                n);
        }
        std::string operator()(P<ReturnStatementV>& n) const {
            return fmt::format("{}Return\n{}", get_indent(indent),
                               ast_to_string(std::move(n->m_expr), indent + 1));
        }
        std::string operator()(ExpressionV& n) const {
            return std::visit(
                [this](auto& p) { return ast_to_string(std::move(p), indent); },
                n);
        }
        std::string operator()(TermV& n) {
            return std::visit(
                [this](auto& p) { return ast_to_string(std::move(p), indent); },
                n);
        }
        std::string operator()(FactorV& n) {
            return std::visit(
                [this](auto& p) { return ast_to_string(std::move(p), indent); },
                n);
        }
        std::string operator()(P<BinaryOpExpressionV>& n) const {
            const auto op_char = [&]() {
                switch (n->m_op_type) {
                    case BinaryOpType::Add:
                        return '+';
                    case BinaryOpType::Subtract:
                        return '-';
                    case BinaryOpType::Multiply:
                        return '*';
                    case BinaryOpType::Divide:
                        return '/';
                    default:
                        std::unreachable();
                }
            }();

            return fmt::format("{}BinaryOpExpr: {}\n{}Left:\n{}\n{}Right:\n{}",
                               get_indent(indent), op_char,
                               get_indent(indent + 1),
                               ast_to_string(std::move(n->m_lhs), indent + 2),
                               get_indent(indent + 1),
                               ast_to_string(std::move(n->m_rhs), indent + 2));
        }
        std::string operator()(P<BinaryOpTermV>& n) const {
            const auto op_char = [&]() {
                switch (n->m_op_type) {
                    case BinaryOpType::Add:
                        return '+';
                    case BinaryOpType::Subtract:
                        return '-';
                    case BinaryOpType::Multiply:
                        return '*';
                    case BinaryOpType::Divide:
                        return '/';
                    default:
                        std::unreachable();
                }
            }();

            return fmt::format("{}BinaryOpExpr: {}\n{}Left:\n{}\n{}Right:\n{}",
                               get_indent(indent), op_char,
                               get_indent(indent + 1),
                               ast_to_string(std::move(n->m_lhs), indent + 2),
                               get_indent(indent + 1),
                               ast_to_string(std::move(n->m_rhs), indent + 2));
        }
        std::string operator()(P<ParenGroupFactorV>& n) const {
            return fmt::format(
                "{}ParenGroup\n{}", get_indent(indent),
                ast_to_string(std::move(n->m_expression), indent + 1));
        }
        std::string operator()(P<UnaryOpFactorV>& n) const {
            const auto op = [&]() {
                switch (n->m_op_type) {
                    case UnaryOpType::LogicalNot:
                        return '!';
                    case UnaryOpType::BitwiseNot:
                        return '~';
                    case UnaryOpType::Negate:
                        return '-';
                }
            }();

            return fmt::format(
                "{}UnaryOp: {}\n{}", get_indent(indent), op,
                ast_to_string(std::move(n->m_factor), indent + 1));
        }
        std::string operator()(P<IntLiteralFactorV>& n) const {
            return fmt::format("{}IntLiteral: {}", get_indent(indent),
                               n->m_literal);
        }
    };

    return std::visit(Visit{indent}, node);
}

}  // namespace parser
