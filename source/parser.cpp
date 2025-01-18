#include "parser.hpp"

#include <fmt/core.h>

#include "lexer.hpp"

namespace parser {

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    std::vector<lexer::Token>::iterator& tokens) {
    auto token = *tokens;
    if (token.m_token_type != lexer::TokenType::int_literal) {
        return std::unexpected(
            "Failed to parse expression: malformed int literal");
    }

    int constant{};
    std::from_chars(token.m_data.data(),
                    token.m_data.data() + token.m_data.size(), constant);

    ++tokens;

    return std::make_unique<ConstantExpression>(constant);
}

std::expected<std::unique_ptr<Statement>, std::string> parse_statement(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::return_keyword) {
        return std::unexpected(
            "Failed to parse statement: missing return keyword");
    }
    ++tokens;
    auto expr = parse_expression(tokens);
    if (!expr) {
        return std::unexpected(expr.error());
    }

    if (tokens->m_token_type != lexer::TokenType::semicolon) {
        return std::unexpected("Failed to parse statement: missing semicolon");
    }
    ++tokens;

    return std::make_unique<ReturnStatement>(std::move(expr.value()));
}

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    std::vector<lexer::Token>::iterator& tokens) {
    if (tokens->m_token_type != lexer::TokenType::int_keyword) {
        return std::unexpected(
            "Failed to parse function: malformed return type");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::identifier) {
        return std::unexpected("Failed to parse function: malformed name");
    }
    auto name = tokens->m_data;
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::open_paren) {
        return std::unexpected("Failed to parse function: missing open paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::close_paren) {
        return std::unexpected(
            "Failed to parse function: missing closing paren");
    }
    ++tokens;

    if (tokens->m_token_type != lexer::TokenType::open_brace) {
        return std::unexpected("Failed to parse function: missing open brace");
    }
    ++tokens;

    auto statement = parse_statement(tokens);

    if (!statement) {
        return std::unexpected(statement.error());
    }

    if (tokens->m_token_type != lexer::TokenType::close_brace) {
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
}  // namespace parser
