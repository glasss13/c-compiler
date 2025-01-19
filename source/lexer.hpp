#pragma once

#include <ranges>
#include <sstream>
#include <string>
#include <vector>

namespace lexer {
enum class TokenType {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    Int,
    Return,
    Identifier,
    IntLiteral,
    Dash,
    Tilde,
    Bang,
    Plus,
    Asterisk,
    ForwardSlash,
};

struct Token {
    TokenType m_token_type;
    std::string m_data;

    Token(TokenType token_type, std::string data)
        : m_token_type(token_type), m_data(std::move(data)) {}
};

[[nodiscard]] std::vector<Token> lex_program(const std::string& src);
[[nodiscard]] std::vector<Token> lex_program(std::istream& istream);

}  // namespace lexer
