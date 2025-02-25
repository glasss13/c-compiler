#include "lexer.hpp"

#include <algorithm>

namespace {
bool is_identifier_char(auto c) {
    return static_cast<bool>(std::isalnum(c)) || c == '_';
}
bool is_identifier_start(auto c) {
    return static_cast<bool>(std::isalpha(c)) || c == '_';
}
bool is_space(auto c) { return static_cast<bool>(std::isspace(c)); }
bool is_digit(auto c) { return static_cast<bool>(std::isdigit(c)); }

}  // namespace

namespace lexer {

[[nodiscard]] bool TokenStream::has_tok(lexer::TokenType token) const {
    return m_tokens[m_idx].m_token_type == token;
}

bool TokenStream::consume_if(lexer::TokenType token) {
    if (m_tokens[m_idx].m_token_type == token) {
        ++m_idx;
        return true;
    }
    return false;
}
[[nodiscard]] std::optional<Token> TokenStream::peek(size_t lookahead) const {
    if (m_idx + lookahead >= m_tokens.size()) {
        return std::nullopt;
    }

    return m_tokens[m_idx + lookahead];
}

std::optional<Token> TokenStream::try_consume(lexer::TokenType token) {
    if (m_tokens[m_idx].m_token_type == token) {
        return m_tokens[m_idx++];
    }
    return std::nullopt;
}

Token TokenStream::consume() { return m_tokens[m_idx++]; }

[[nodiscard]] TokenStream lex_program(const std::string& src) {
    std::stringstream ss(src);
    return lex_program(ss);
}

[[nodiscard]] TokenStream lex_program(std::istream& istream) {  // NOLINT
    using namespace std::literals;
    namespace ranges = std::ranges;

    std::vector<Token> out;

    std::string cur_token;
    char cur_char{};

    while (istream.get(cur_char)) {
        if (is_space(cur_char)) {
            continue;
        }
        cur_token += cur_char;

        if (cur_token == "{"sv) {
            out.emplace_back(TokenType::OpenBrace, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "}"sv) {
            out.emplace_back(TokenType::CloseBrace, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "("sv) {
            out.emplace_back(TokenType::OpenParen, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ")"sv) {
            out.emplace_back(TokenType::CloseParen, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ";"sv) {
            out.emplace_back(TokenType::Semicolon, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ","sv) {
            out.emplace_back(TokenType::Comma, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "int"sv && is_space(istream.peek())) {
            out.emplace_back(TokenType::Int, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "return"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::Return, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "-"sv && istream.peek() != '=' &&
                   istream.peek() != '-') {
            out.emplace_back(TokenType::Minus, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "--"sv) {
            out.emplace_back(TokenType::MinusMinus, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "-=") {
            out.emplace_back(TokenType::MinusEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "~"sv) {
            out.emplace_back(TokenType::Tilde, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "!"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::Bang, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "+"sv && istream.peek() != '=' &&
                   istream.peek() != '+') {
            out.emplace_back(TokenType::Plus, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "++"sv) {
            out.emplace_back(TokenType::PlusPlus, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "+="sv) {
            out.emplace_back(TokenType::PlusEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "*"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::Asterisk, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "*="sv) {
            out.emplace_back(TokenType::AsteriskEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "/"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::ForwardSlash, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "/="sv) {
            out.emplace_back(TokenType::ForwardSlashEqual,
                             std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "&"sv && istream.peek() != '&' &&
                   istream.peek() != '=') {
            out.emplace_back(TokenType::Ampersan, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "&="sv) {
            out.emplace_back(TokenType::AmpersanEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "&&"sv) {
            out.emplace_back(TokenType::DoubleAmpersan, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "|"sv && istream.peek() != '|' &&
                   istream.peek() != '=') {
            out.emplace_back(TokenType::Or, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "|="sv) {
            out.emplace_back(TokenType::OrEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "||"sv) {
            out.emplace_back(TokenType::DoubleOr, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "="sv && istream.peek() != '=') {
            out.emplace_back(TokenType::Equal, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "=="sv) {
            out.emplace_back(TokenType::DoubleEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "!=") {
            out.emplace_back(TokenType::NotEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "<"sv && istream.peek() != '=' &&
                   istream.peek() != '<') {
            out.emplace_back(TokenType::LessThan, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "<<"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::DoubleLessThan, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "<<="sv) {
            out.emplace_back(TokenType::DoubleLessThanEqual,
                             std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "<="sv) {
            out.emplace_back(TokenType::LessThanOrEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ">"sv && istream.peek() != '=' &&
                   istream.peek() != '>') {
            out.emplace_back(TokenType::GreaterThan, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ">>"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::DoubleGreaterThan,
                             std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ">>="sv) {
            out.emplace_back(TokenType::DoubleGreaterThanEqual,
                             std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ">="sv) {
            out.emplace_back(TokenType::GreaterThanOrEqual,
                             std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "%"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::Percent, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "%="sv) {
            out.emplace_back(TokenType::PercentEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "^"sv && istream.peek() != '=') {
            out.emplace_back(TokenType::Caret, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "^="sv) {
            out.emplace_back(TokenType::CaretEqual, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "if"sv && !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::If, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "else"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::Else, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "for"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::For, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "while"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::While, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "do"sv && !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::Do, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "break"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::Break, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "continue"sv &&
                   !is_identifier_char(istream.peek())) {
            out.emplace_back(TokenType::Continue, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ":"sv) {
            out.emplace_back(TokenType::Colon, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "?"sv) {
            out.emplace_back(TokenType::QuestionMark, std::move(cur_token));
            cur_token.clear();
        } else if (!is_digit(istream.peek()) &&
                   ranges::all_of(cur_token, is_digit<char>)) {
            out.emplace_back(TokenType::IntLiteral, std::move(cur_token));
            cur_token.clear();
        } else if (!is_identifier_char(istream.peek()) &&
                   ranges::all_of(cur_token, is_identifier_char<char>)) {
            out.emplace_back(TokenType::Identifier, std::move(cur_token));
            cur_token.clear();
        }
    }

    return TokenStream(out);
}
}  // namespace lexer
