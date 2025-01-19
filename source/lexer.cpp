#include "lexer.hpp"

#include <algorithm>

namespace {
bool is_identifier_char(auto c) {
    return static_cast<bool>(std::isalnum(c)) || c == '_';
}
bool is_space(auto c) { return static_cast<bool>(std::isspace(c)); }
bool is_digit(auto c) { return static_cast<bool>(std::isdigit(c)); }

}  // namespace

namespace lexer {

[[nodiscard]] std::vector<Token> lex_stream(std::istream& istream) {
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
        } else if (cur_token == "int"sv && is_space(istream.peek())) {
            out.emplace_back(TokenType::Int, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "return"sv && is_space(istream.peek())) {
            out.emplace_back(TokenType::Return, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "-"sv) {
            out.emplace_back(TokenType::Dash, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "~"sv) {
            out.emplace_back(TokenType::Tilde, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "!"sv) {
            out.emplace_back(TokenType::Bang, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "+"sv) {
            out.emplace_back(TokenType::Plus, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "*"sv) {
            out.emplace_back(TokenType::Asterisk, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "/"sv) {
            out.emplace_back(TokenType::ForwardSlash, std::move(cur_token));
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

    return out;
}
}  // namespace lexer
