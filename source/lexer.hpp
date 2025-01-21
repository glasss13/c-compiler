#pragma once

#include <fmt/format.h>

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
    Ampersan,
    DoubleAmpersan,
    Or,
    DoubleOr,
    Equal,
    DoubleEqual,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    DoubleGreaterThan,
    DoubleLessThan,
    Percent,
    Caret,
};

struct Token {
    TokenType m_token_type;
    std::string m_data;

    Token(TokenType token_type, std::string data)
        : m_token_type(token_type), m_data(std::move(data)) {}

    std::string to_string() const {
        switch (m_token_type) {
            case TokenType::OpenBrace:
                return "OpenBrace";
            case TokenType::CloseBrace:
                return "CloseBrace";
            case TokenType::OpenParen:
                return "OpenParen";
            case TokenType::CloseParen:
                return "CloseParen";
            case TokenType::Semicolon:
                return "Semicolon";
            case TokenType::Int:
                return "Int";
            case TokenType::Return:
                return "Return";
            case TokenType::Identifier:
                return fmt::format("Identifier({})", m_data);
            case TokenType::IntLiteral:
                return fmt::format("IntLiteral({})", m_data);
            case TokenType::Dash:
                return "Dash";
            case TokenType::Tilde:
                return "Tilde";
            case TokenType::Bang:
                return "Bang";
            case TokenType::Plus:
                return "Plus";
            case TokenType::Asterisk:
                return "Asterisk";
            case TokenType::ForwardSlash:
                return "ForwardSlash";
            case TokenType::Ampersan:
                return "Ampersan";
            case TokenType::DoubleAmpersan:
                return "DoubleAmpersan";
            case TokenType::Or:
                return "Or";
            case TokenType::DoubleOr:
                return "DoubleOr";
            case TokenType::Equal:
                return "Equal";
            case TokenType::DoubleEqual:
                return "DoubleEqual";
            case TokenType::NotEqual:
                return "NotEqual";
            case TokenType::LessThan:
                return "LessThan";
            case TokenType::LessThanOrEqual:
                return "LessThanOrEqual";
            case TokenType::GreaterThan:
                return "GreaterThan";
            case TokenType::GreaterThanOrEqual:
                return "GreaterThanOrEqual";
            case TokenType::DoubleGreaterThan:
                return "DoubleGreaterThan";
            case TokenType::DoubleLessThan:
                return "DoubleLessThan";
            case TokenType::Percent:
                return "Percent";
            case TokenType::Caret:
                return "Caret";
        }
    }
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
    [[nodiscard]] size_t save() const { return m_idx; }
    void restore(size_t idx) { m_idx = idx; }

private:
    std::vector<Token> m_tokens;
    size_t m_idx{0};
};

[[nodiscard]] TokenStream lex_program(const std::string& src);
[[nodiscard]] TokenStream lex_program(std::istream& istream);

}  // namespace lexer
