#include "parser.hpp"

#include <fmt/core.h>

#include "lexer.hpp"

namespace {

inline std::string get_indent(int level) {
    std::string out(static_cast<size_t>(level * 2), ' ');
    return out;
}

parser::BinaryOpType bin_op_type(lexer::TokenType token) {
    switch (token) {
        case lexer::TokenType::Dash:
            return parser::BinaryOpType::Subtract;
        case lexer::TokenType::Plus:
            return parser::BinaryOpType::Add;
        case lexer::TokenType::Asterisk:
            return parser::BinaryOpType::Multiply;
        case lexer::TokenType::ForwardSlash:
            return parser::BinaryOpType::Divide;
        case lexer::TokenType::DoubleAnd:
            return parser::BinaryOpType::LogicalAnd;
        case lexer::TokenType::DoubleOr:
            return parser::BinaryOpType::LogicalOr;
        case lexer::TokenType::DoubleEqual:
            return parser::BinaryOpType::Equal;
        case lexer::TokenType::NotEqual:
            return parser::BinaryOpType::NotEqual;
        case lexer::TokenType::LessThan:
            return parser::BinaryOpType::LessThan;
        case lexer::TokenType::LessThanOrEqual:
            return parser::BinaryOpType::LessThanOrEqual;
        case lexer::TokenType::GreaterThan:
            return parser::BinaryOpType::GreaterThan;
        case lexer::TokenType::GreaterThanOrEqual:
            return parser::BinaryOpType::GreaterThanOrEqual;
        case lexer::TokenType::Percent:
            return parser::BinaryOpType::Modulo;
        case lexer::TokenType::OpenBrace:
        case lexer::TokenType::CloseBrace:
        case lexer::TokenType::OpenParen:
        case lexer::TokenType::CloseParen:
        case lexer::TokenType::Semicolon:
        case lexer::TokenType::Int:
        case lexer::TokenType::Return:
        case lexer::TokenType::Identifier:
        case lexer::TokenType::IntLiteral:
        case lexer::TokenType::Bang:
        case lexer::TokenType::Tilde:
            std::unreachable();
    }
}
}  // namespace

namespace parser {

using lexer::TokenType;

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    lexer::TokenStream token_stream) {
    auto function = parse_function(token_stream);
    if (!function) {
        return std::unexpected(function.error());
    }

    return std::make_unique<Program>(std::move(function.value()));
}

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    lexer::TokenStream& token_stream) {
    if (!token_stream.consume_if(TokenType::Int)) {
        return std::unexpected(
            "Failed to parse function: malformed return type");
    }

    const auto id_token = token_stream.try_consume(TokenType::Identifier);
    if (!id_token) {
        return std::unexpected(
            "Failed to parse function: malformed identifier");
    }

    if (!token_stream.consume_if(TokenType::OpenParen)) {
        return std::unexpected("Failed to parse function: missing open paren");
    }
    if (!token_stream.consume_if(TokenType::CloseParen)) {
        return std::unexpected(
            "Failed to parse function: missing closing paren");
    }

    if (!token_stream.consume_if(TokenType::OpenBrace)) {
        return std::unexpected("Failed to parse function: missing open brace");
    }

    auto statement = parse_statement(token_stream);
    if (!statement) {
        return std::unexpected(statement.error());
    }

    if (!token_stream.consume_if(TokenType::CloseBrace)) {
        return std::unexpected(
            "Failed to parse function: missing closing brace");
    }

    return std::make_unique<Function>(id_token->m_data,
                                      std::move(statement.value()));
}

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    lexer::TokenStream& token_stream) {
    if (!token_stream.consume_if(TokenType::Return)) {
        return std::unexpected(
            "Failed to parse statement: missing return keyword");
    }

    auto expr = parse_expression(token_stream);
    if (!expr) {
        return std::unexpected(expr.error());
    }

    if (!token_stream.consume_if(TokenType::Semicolon)) {
        return std::unexpected("Failed to parse statement: missing semicolon");
    }

    return std::make_unique<ReturnStatement>(std::move(expr.value()));
}

template <auto child_parse, lexer::TokenType... Tokens>
std::expected<std::unique_ptr<Expression>, std::string> parse_expression_t(
    lexer::TokenStream& token_stream) {
    auto maybe_expr = child_parse(token_stream);
    if (!maybe_expr) {
        return std::unexpected(maybe_expr.error());
    }
    std::unique_ptr<Expression> expr = std::move(maybe_expr.value());

    while ((token_stream.has_tok(Tokens) || ...)) {
        const auto op_token = token_stream.consume();

        auto next_expr = child_parse(token_stream);
        if (!next_expr) {
            return std::unexpected(next_expr.error());
        }
        expr = std::make_unique<BinaryOpExpression>(
            std::move(expr), std::move(*next_expr),
            bin_op_type(op_token.m_token_type));
    }

    return expr;
}

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_logical_and_expr, TokenType::DoubleOr>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_logical_and_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_equality_expression, TokenType::DoubleAnd>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_equality_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_relational_expression, TokenType::NotEqual,
                              TokenType::DoubleEqual>(token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_relational_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<
        parse_additive_expression, TokenType::LessThan, TokenType::GreaterThan,
        TokenType::LessThanOrEqual, TokenType::GreaterThanOrEqual>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_additive_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_term, TokenType::Plus, TokenType::Dash>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_factor(
    lexer::TokenStream& token_stream) {
    if (const auto token = token_stream.try_consume(TokenType::IntLiteral)) {
        int constant{};
        std::from_chars(token->m_data.data(),
                        token->m_data.data() + token->m_data.size(), constant);

        return std::make_unique<IntLiteralExpression>(constant);
    }

    if (const auto token = token_stream.try_consume(TokenType::OpenParen)) {
        auto child_expr = parse_expression(token_stream);
        if (!child_expr) {
            return std::unexpected(child_expr.error());
        }
        if (!token_stream.try_consume(TokenType::CloseParen)) {
            return std::unexpected(
                "Failed to parse factor: expected closing paren");
        }
        return child_expr;
    }

    const auto handle_unary_op = [&](UnaryOpType op_type)
        -> std::expected<std::unique_ptr<UnaryOpExpression>, std::string> {
        auto child_expr = parse_factor(token_stream);
        if (!child_expr) {
            return std::unexpected(child_expr.error());
        }
        return std::make_unique<UnaryOpExpression>(
            std::move(child_expr.value()), op_type);
    };

    if (const auto token = token_stream.try_consume(TokenType::Tilde)) {
        return handle_unary_op(UnaryOpType::BitwiseNot);
    }
    if (const auto token = token_stream.try_consume(TokenType::Bang)) {
        return handle_unary_op(UnaryOpType::LogicalNot);
    }
    if (const auto token = token_stream.try_consume(TokenType::Dash)) {
        return handle_unary_op(UnaryOpType::Negate);
    }

    return std::unexpected(
        fmt::format("Failed to parse factor: expected integer literal or "
                    "unary operator, found: {}",
                    token_stream.peek()
                        .transform([](const auto& s) { return s.m_data; })
                        .value_or("EOF")));
}

std::expected<std::unique_ptr<Expression>, std::string> parse_term(
    lexer::TokenStream& token_stream) {
    auto maybe_factor = parse_factor(token_stream);
    if (!maybe_factor) {
        return std::unexpected(maybe_factor.error());
    }

    std::unique_ptr<Expression> factor = std::move(maybe_factor.value());

    while (token_stream.has_tok(TokenType::Asterisk) ||
           token_stream.has_tok(TokenType::ForwardSlash) ||
           token_stream.has_tok(TokenType::Percent)) {
        const auto op_token = token_stream.consume();

        auto next_factor = parse_factor(token_stream);
        if (!next_factor) {
            return std::unexpected(next_factor.error());
        }

        factor = std::make_unique<BinaryOpExpression>(
            std::move(factor), std::move(next_factor.value()),
            bin_op_type(op_token.m_token_type));
    }

    return factor;
}

[[nodiscard]] std::string BinaryOpExpression::to_string(int indent) const {
    using namespace std::literals;
    const auto op_str = [&]() {
        switch (m_op_type) {
            case BinaryOpType::Add:
                return "+"sv;
            case BinaryOpType::Subtract:
                return "-"sv;
            case BinaryOpType::Multiply:
                return "*"sv;
            case BinaryOpType::Divide:
                return "/"sv;
            case BinaryOpType::LessThan:
                return "<"sv;
            case BinaryOpType::GreaterThan:
                return ">"sv;
            case BinaryOpType::LessThanOrEqual:
                return "<="sv;
            case BinaryOpType::GreaterThanOrEqual:
                return ">="sv;
            case BinaryOpType::Equal:
                return "=="sv;
            case BinaryOpType::NotEqual:
                return "!="sv;
            case BinaryOpType::LogicalAnd:
                return "&&"sv;
            case BinaryOpType::LogicalOr:
                return "||"sv;
            case BinaryOpType::Modulo:
                return "%"sv;
        }
    }();

    return fmt::format("{}BinaryOpExpr: {}\n{}Left:\n{}\n{}Right:\n{}",
                       get_indent(indent), op_str, get_indent(indent + 1),
                       m_lhs->to_string(indent + 2), get_indent(indent + 1),
                       m_rhs->to_string(indent + 2));
}

[[nodiscard]] std::string IntLiteralExpression::to_string(int indent) const {
    return fmt::format("{}IntLiteral: {}", get_indent(indent), m_literal);
}

[[nodiscard]] std::string UnaryOpExpression::to_string(int indent) const {
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
                       m_expr->to_string(indent + 1));
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

// [[nodiscard]] std::expected<FactorV, std::string> parse_factor_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//     auto token = *tokens;
//     ++tokens;
//
//     switch (token.m_token_type) {
//         case TokenType::IntLiteral: {
//             int constant{};
//             std::from_chars(token.m_data.data(),
//                             token.m_data.data() + token.m_data.size(),
//                             constant);
//
//             return std::make_unique<IntLiteralFactorV>(constant);
//         }
//         case TokenType::Tilde:
//         case TokenType::Bang:
//         case TokenType::Dash: {
//             auto child_expr = parse_factor_v(tokens);
//             if (!child_expr) {
//                 return std::unexpected(child_expr.error());
//             }
//
//             const auto type = token.m_token_type == TokenType::Tilde
//                                   ? UnaryOpType::BitwiseNot
//                               : token.m_token_type == TokenType::Bang
//                                   ? UnaryOpType::LogicalNot
//                                   : UnaryOpType::Negate;
//
//             return std::make_unique<UnaryOpFactorV>(
//                 std::move(child_expr.value()), type);
//         }
//         case TokenType::OpenParen: {
//             auto child_expr = parse_expression_v(tokens);
//             if (!child_expr) {
//                 return std::unexpected(child_expr.error());
//             }
//             if (tokens->m_token_type != TokenType::CloseParen) {
//                 return std::unexpected(
//                     "Failed to parse factor: expected closing paren");
//             }
//             ++tokens;
//             return std::make_unique<ParenGroupFactorV>(
//                 std::move(child_expr.value()));
//         }
//         default:
//             return std::unexpected(fmt::format(
//                 "Failed to parse factor: expected integer literal or "
//                 "unary operator, received: {}",
//                 token.m_data));
//     }
// }
//
// [[nodiscard]] std::expected<TermV, std::string> parse_term_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//
//     auto maybe_factor = parse_factor_v(tokens);
//     if (!maybe_factor) {
//         return std::unexpected(maybe_factor.error());
//     }
//
//     TermV factor = std::move(maybe_factor.value());
//
//     while (tokens->m_token_type == TokenType::Asterisk ||
//            tokens->m_token_type == TokenType::ForwardSlash) {
//         auto op_type = tokens->m_token_type == TokenType::Asterisk
//                            ? BinaryOpType::Multiply
//                            : BinaryOpType::Divide;
//         ++tokens;
//
//         auto next_factor = parse_factor_v(tokens);
//         if (!next_factor) {
//             return std::unexpected(next_factor.error());
//         }
//
//         factor = std::make_unique<BinaryOpExpressionV>(
//             std::move(factor), std::move(next_factor.value()), op_type);
//     }
//
//     return factor;
// }
//
// [[nodiscard]] std::expected<ExpressionV, std::string> parse_expression_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//
//     auto maybe_term = parse_term_v(tokens);
//     if (!maybe_term) {
//         return std::unexpected(maybe_term.error());
//     }
//     ExpressionV term = std::move(maybe_term.value());
//
//     while (tokens->m_token_type == TokenType::Plus ||
//            tokens->m_token_type == TokenType::Dash) {
//         auto op_type = tokens->m_token_type == TokenType::Plus
//                            ? BinaryOpType::Add
//                            : BinaryOpType::Subtract;
//         ++tokens;
//
//         auto next_term = parse_term_v(tokens);
//         if (!next_term) {
//             return std::unexpected(next_term.error());
//         }
//         term = std::make_unique<BinaryOpExpressionV>(
//             std::move(term), std::move(next_term.value()), op_type);
//     }
//
//     return term;
// }
//
// [[nodiscard]] std::expected<StatementV, std::string> parse_statement_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     if (tokens->m_token_type != lexer::TokenType::Return) {
//         return std::unexpected(
//             "Failed to parse statement: missing return keyword");
//     }
//     ++tokens;
//     auto expr = parse_expression_v(tokens);
//     if (!expr) {
//         return std::unexpected(expr.error());
//     }
//
//     if (tokens->m_token_type != lexer::TokenType::Semicolon) {
//         return std::unexpected("Failed to parse statement: missing
//         semicolon");
//     }
//     ++tokens;
//
//     return std::make_unique<ReturnStatementV>(std::move(expr.value()));
// }
//
// [[nodiscard]] std::expected<P<FunctionV>, std::string> parse_function_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     if (tokens->m_token_type != lexer::TokenType::Int) {
//         return std::unexpected(
//             "Failed to parse function: malformed return type");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::Identifier) {
//         return std::unexpected("Failed to parse function: malformed name");
//     }
//     auto name = tokens->m_data;
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::OpenParen) {
//         return std::unexpected("Failed to parse function: missing open
//         paren");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::CloseParen) {
//         return std::unexpected(
//             "Failed to parse function: missing closing paren");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::OpenBrace) {
//         return std::unexpected("Failed to parse function: missing open
//         brace");
//     }
//     ++tokens;
//
//     auto statement = parse_statement_v(tokens);
//
//     if (!statement) {
//         return std::unexpected(statement.error());
//     }
//
//     if (tokens->m_token_type != lexer::TokenType::CloseBrace) {
//         return std::unexpected(
//             "Failed to parse function: missing closing brace");
//     }
//     ++tokens;
//
//     return std::make_unique<FunctionV>(name, std::move(statement.value()));
// }
//
// [[nodiscard]] std::expected<P<ProgramV>, std::string> parse_program_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     auto function = parse_function_v(tokens);
//     if (!function) {
//         return std::unexpected(function.error());
//     }
//
//     return std::make_unique<ProgramV>(std::move(function.value()));
// }
//
// std::string ast_to_string(AstNodeV node, int indent) {
//     struct Visit {
//         int indent;
//         std::string operator()(P<ProgramV>& n) const {
//             return fmt::format(
//                 "{}Program\n{}", get_indent(indent),
//                 ast_to_string(std::move(n->m_function), indent + 1));
//         }
//         std::string operator()(P<FunctionV>& n) const {
//             return fmt::format(
//                 "{}Function: {}\n{}", get_indent(indent), n->m_name,
//                 ast_to_string(std::move(n->m_statement), indent + 1));
//         }
//         std::string operator()(StatementV& n) const {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p), indent);
//                 }, n);
//         }
//         std::string operator()(P<ReturnStatementV>& n) const {
//             return fmt::format("{}Return\n{}", get_indent(indent),
//                                ast_to_string(std::move(n->m_expr), indent +
//                                1));
//         }
//         std::string operator()(ExpressionV& n) const {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p), indent);
//                 }, n);
//         }
//         std::string operator()(TermV& n) {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p), indent);
//                 }, n);
//         }
//         std::string operator()(FactorV& n) {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p), indent);
//                 }, n);
//         }
//         std::string operator()(P<BinaryOpExpressionV>& n) const {
//             const auto op_char = [&]() {
//                 switch (n->m_op_type) {
//                     case BinaryOpType::Add:
//                         return '+';
//                     case BinaryOpType::Subtract:
//                         return '-';
//                     case BinaryOpType::Multiply:
//                         return '*';
//                     case BinaryOpType::Divide:
//                         return '/';
//                     default:
//                         std::unreachable();
//                 }
//             }();
//
//             return fmt::format("{}BinaryOp: {}\n{}Left:\n{}\n{}Right:\n{}",
//                                get_indent(indent), op_char,
//                                get_indent(indent + 1),
//                                ast_to_string(std::move(n->m_lhs), indent +
//                                2), get_indent(indent + 1),
//                                ast_to_string(std::move(n->m_rhs), indent +
//                                2));
//         }
//         std::string operator()(P<ParenGroupFactorV>& n) const {
//             return fmt::format(
//                 "{}ParenGroup\n{}", get_indent(indent),
//                 ast_to_string(std::move(n->m_expression), indent + 1));
//         }
//         std::string operator()(P<UnaryOpFactorV>& n) const {
//             const auto op = [&]() {
//                 switch (n->m_op_type) {
//                     case UnaryOpType::LogicalNot:
//                         return '!';
//                     case UnaryOpType::BitwiseNot:
//                         return '~';
//                     case UnaryOpType::Negate:
//                         return '-';
//                 }
//             }();
//
//             return fmt::format(
//                 "{}UnaryOp: {}\n{}", get_indent(indent), op,
//                 ast_to_string(std::move(n->m_factor), indent + 1));
//         }
//         std::string operator()(P<IntLiteralFactorV>& n) const {
//             return fmt::format("{}IntLiteral: {}", get_indent(indent),
//                                n->m_literal);
//         }
//     };
//
//     return std::visit(Visit{indent}, node);
// }

}  // namespace parser
