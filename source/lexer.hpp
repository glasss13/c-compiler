#pragma once

#include <ranges>
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
    negation,
    bitwise_not,
    logical_not,
};

struct Token {
    TokenType m_token_type;
    std::string m_data;

    Token(TokenType token_type, std::string data)
        : m_token_type(token_type), m_data(std::move(data)) {}
};

[[nodiscard]] std::vector<Token> lex_stream(std::istream& istream);

}  // namespace lexer
