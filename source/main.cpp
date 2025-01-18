#include <fstream>
#include <iostream>
#include <string>

#include "lexer.hpp"
#include "lib.hpp"

int main() {
    std::ifstream src("return_2.c");

    auto tokens = lexer::parse_token_stream(src);
    if (!tokens) {
        std::cout << "error\n";
        return 1;
    }

    for (const auto& token : tokens.value()) {
        std::cout << '[' << token.m_data << ']' << '\n';
    }

    return 0;
}
