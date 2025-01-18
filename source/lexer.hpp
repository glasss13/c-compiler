#pragma once

#include <expected>
#include <sstream>
#include <string>
#include <vector>

namespace lexer {

enum class TokenType {
    open_brace,
    close_brace,
    open_paren,
    close_paren,
    semicolon,
    int_keyword,
    return_keyword,
    identifier,
    int_literal,
};

struct Token {
    TokenType m_token_type;
    std::string m_data;

    Token(TokenType token_type, std::string data)
        : m_token_type(token_type), m_data(std::move(data)) {}
};

[[nodiscard]] std::expected<std::vector<Token>, std::string> parse_token_stream(
    std::istream& istream);

}  // namespace lexer
