#include "lexer.hpp"

#include <__algorithm/ranges_all_of.h>

#include <cctype>
#include <charconv>
#include <iostream>
#include <istream>
#include <ranges>

namespace {
bool is_identifier_char(char c) {
    return static_cast<bool>(std::isalnum(c)) || c == '_';
}
}  // namespace

namespace lexer {
namespace ranges = std::ranges;

[[nodiscard]] std::expected<std::vector<Token>, std::string> parse_token_stream(
    std::istream& istream) {
    using namespace std::literals;

    std::vector<Token> out;

    std::string cur_token;
    char cur_char{};

    while (istream.get(cur_char)) {
        if (static_cast<bool>(std::isspace(cur_char))) {
            continue;
        }
        cur_token += cur_char;

        if (cur_token == "{"sv) {
            out.emplace_back(TokenType::open_brace, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "}"sv) {
            out.emplace_back(TokenType::close_brace, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "("sv) {
            out.emplace_back(TokenType::open_paren, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ")"sv) {
            out.emplace_back(TokenType::close_paren, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == ";"sv) {
            out.emplace_back(TokenType::semicolon, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "int"sv) {
            out.emplace_back(TokenType::int_keyword, std::move(cur_token));
            cur_token.clear();
        } else if (cur_token == "return"sv) {
            out.emplace_back(TokenType::return_keyword, std::move(cur_token));
            cur_token.clear();
        } else if (std::isdigit(istream.peek()) == 0 &&
                   ranges::all_of(cur_token,
                                  [](char c) { return std::isdigit(c); })) {
            out.emplace_back(TokenType::int_literal, std::move(cur_token));
            cur_token.clear();
        } else if (!is_identifier_char(static_cast<char>(istream.peek())) &&
                   ranges::all_of(cur_token, is_identifier_char)) {
            out.emplace_back(TokenType::identifier, std::move(cur_token));
            cur_token.clear();
        }
    }

    return out;
}
}  // namespace lexer
