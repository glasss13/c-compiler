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

class TokenStream {
public:
    explicit TokenStream(std::vector<Token> tokens)
        : m_tokens(std::move(tokens)) {}

    [[nodiscard]] bool has_tok(lexer::TokenType token) const;
    bool consume_if(lexer::TokenType token);
    [[nodiscard]] std::optional<Token> peek(size_t lookahead = 0) const;
    std::optional<Token> try_consume(lexer::TokenType token);
    Token consume();
    [[nodiscard]] auto begin() const { return m_tokens.cbegin(); }
    [[nodiscard]] auto end() const { return m_tokens.cend(); }
    [[nodiscard]] const auto& operator[](size_t idx) const {
        return m_tokens.at(idx);
    }

private:
    std::vector<Token> m_tokens;
    size_t m_idx{0};
};

[[nodiscard]] TokenStream lex_program(const std::string& src);
[[nodiscard]] TokenStream lex_program(std::istream& istream);

}  // namespace lexer
