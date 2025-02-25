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
    Minus,
    MinusMinus,
    MinusEqual,
    Tilde,
    Bang,
    Plus,
    PlusPlus,
    PlusEqual,
    Asterisk,
    AsteriskEqual,
    ForwardSlash,
    ForwardSlashEqual,
    Ampersan,
    DoubleAmpersan,
    AmpersanEqual,
    Or,
    Comma,
    DoubleOr,
    OrEqual,
    Equal,
    DoubleEqual,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    DoubleGreaterThan,
    DoubleGreaterThanEqual,
    DoubleLessThan,
    DoubleLessThanEqual,
    Percent,
    PercentEqual,
    Caret,
    CaretEqual,
    If,
    Else,
    Colon,
    QuestionMark,
    For,
    While,
    Do,
    Break,
    Continue,
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
            case TokenType::Minus:
                return "Minus";
            case TokenType::MinusMinus:
                return "MinusMinus";
            case TokenType::MinusEqual:
                return "MinusEqual";
            case TokenType::Tilde:
                return "Tilde";
            case TokenType::Bang:
                return "Bang";
            case TokenType::Plus:
                return "Plus";
            case TokenType::PlusPlus:
                return "PlusPlus";
            case TokenType::PlusEqual:
                return "PlusEqual";
            case TokenType::Asterisk:
                return "Asterisk";
            case TokenType::AsteriskEqual:
                return "AsteriskEqual";
            case TokenType::ForwardSlash:
                return "ForwardSlash";
            case TokenType::ForwardSlashEqual:
                return "ForwardSlashEqual";
            case TokenType::Ampersan:
                return "Ampersan";
            case TokenType::DoubleAmpersan:
                return "DoubleAmpersan";
            case TokenType::AmpersanEqual:
                return "AmpersanEqual";
            case TokenType::Or:
                return "Or";
            case TokenType::Comma:
                return "Comma";
            case TokenType::DoubleOr:
                return "DoubleOr";
            case TokenType::OrEqual:
                return "OrEqual";
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
            case TokenType::DoubleGreaterThanEqual:
                return "DoubleGreaterThanEqual";
            case TokenType::DoubleLessThan:
                return "DoubleLessThan";
            case TokenType::DoubleLessThanEqual:
                return "DoubleLessThanEqual";
            case TokenType::Percent:
                return "Percent";
            case TokenType::PercentEqual:
                return "PercentEqual";
            case TokenType::Caret:
                return "Caret";
            case TokenType::CaretEqual:
                return "CaretEqual";
            case TokenType::If:
                return "If";
            case TokenType::Else:
                return "Else";
            case TokenType::Colon:
                return "Colon";
            case TokenType::QuestionMark:
                return "QuestionMark";
            case TokenType::For:
                return "For";
            case TokenType::While:
                return "While";
            case TokenType::Do:
                return "Do";
            case TokenType::Break:
                return "Break";
            case TokenType::Continue:
                return "Continue";
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
