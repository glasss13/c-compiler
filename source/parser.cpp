#include "parser.hpp"

#include <fmt/core.h>

#include <iostream>

#include "lexer.hpp"

namespace {

inline std::string get_indent(int level) {
    std::string out(static_cast<size_t>(level * 2), ' ');
    return out;
}

std::optional<parser::CompoundAssignmentType> compound_assign_type(
    lexer::TokenType token) {
    using lexer::TokenType;
    using parser::CompoundAssignmentType;
    switch (token) {
        case TokenType::OrEqual:
            return CompoundAssignmentType::BitwiseOrEqual;
        case TokenType::AmpersanEqual:
            return CompoundAssignmentType::BitwiseAndEqual;
        case TokenType::CaretEqual:
            return CompoundAssignmentType::XorEqual;
        case TokenType::DoubleLessThanEqual:
            return CompoundAssignmentType::LeftShiftEqual;
        case TokenType::DoubleGreaterThanEqual:
            return CompoundAssignmentType::RightShiftEqual;
        case TokenType::PlusEqual:
            return CompoundAssignmentType::PlusEqual;
        case TokenType::MinusEqual:
            return CompoundAssignmentType::MinusEqual;
        case TokenType::AsteriskEqual:
            return CompoundAssignmentType::TimesEqual;
        case TokenType::ForwardSlashEqual:
            return CompoundAssignmentType::DivideEqual;
        case TokenType::PercentEqual:
            return CompoundAssignmentType::ModuloEqual;
        case TokenType::OpenBrace:
        case TokenType::CloseBrace:
        case TokenType::OpenParen:
        case TokenType::CloseParen:
        case TokenType::Semicolon:
        case TokenType::Int:
        case TokenType::Return:
        case TokenType::Identifier:
        case TokenType::IntLiteral:
        case TokenType::Minus:
        case TokenType::Tilde:
        case TokenType::Bang:
        case TokenType::Plus:
        case TokenType::Asterisk:
        case TokenType::ForwardSlash:
        case TokenType::Ampersan:
        case TokenType::DoubleAmpersan:
        case TokenType::Or:
        case TokenType::DoubleOr:
        case TokenType::Equal:
        case TokenType::DoubleEqual:
        case TokenType::NotEqual:
        case TokenType::LessThan:
        case TokenType::LessThanOrEqual:
        case TokenType::GreaterThan:
        case TokenType::GreaterThanOrEqual:
        case TokenType::DoubleGreaterThan:
        case TokenType::DoubleLessThan:
        case TokenType::Caret:
        case TokenType::Comma:
        case TokenType::MinusMinus:
        case TokenType::PlusPlus:
        case TokenType::Else:
        case TokenType::If:
        case TokenType::Colon:
        case TokenType::QuestionMark:
        case TokenType::For:
        case TokenType::While:
        case TokenType::Do:
        case TokenType::Continue:
        case TokenType::Break:
        case TokenType::Percent:
            return std::nullopt;
    }
}

parser::BinaryOpType bin_op_type(lexer::TokenType token) {
    using lexer::TokenType;
    using parser::BinaryOpType;
    switch (token) {
        case TokenType::Minus:
            return BinaryOpType::Subtract;
        case TokenType::Plus:
            return BinaryOpType::Add;
        case TokenType::Asterisk:
            return BinaryOpType::Multiply;
        case TokenType::ForwardSlash:
            return BinaryOpType::Divide;
        case TokenType::Ampersan:
            return BinaryOpType::BitwiseAnd;
        case TokenType::DoubleAmpersan:
            return BinaryOpType::LogicalAnd;
        case TokenType::Or:
            return BinaryOpType::BitwiseOr;
        case TokenType::DoubleOr:
            return BinaryOpType::LogicalOr;
        case TokenType::DoubleEqual:
            return BinaryOpType::Equal;
        case TokenType::NotEqual:
            return BinaryOpType::NotEqual;
        case TokenType::LessThan:
            return BinaryOpType::LessThan;
        case TokenType::LessThanOrEqual:
            return BinaryOpType::LessThanOrEqual;
        case TokenType::GreaterThan:
            return BinaryOpType::GreaterThan;
        case TokenType::GreaterThanOrEqual:
            return BinaryOpType::GreaterThanOrEqual;
        case TokenType::Percent:
            return BinaryOpType::Modulo;
        case TokenType::Caret:
            return BinaryOpType::BitwiseXor;
        case lexer::TokenType::DoubleGreaterThan:
            return BinaryOpType::RightShift;
        case lexer::TokenType::DoubleLessThan:
            return BinaryOpType::LeftShift;
        case TokenType::Comma:
            return BinaryOpType::Comma;
        case TokenType::OpenBrace:
        case TokenType::CloseBrace:
        case TokenType::OpenParen:
        case TokenType::CloseParen:
        case TokenType::Semicolon:
        case TokenType::Int:
        case TokenType::Return:
        case TokenType::Identifier:
        case TokenType::IntLiteral:
        case TokenType::Bang:
        case TokenType::Tilde:
        case TokenType::Equal:
        case TokenType::MinusEqual:
        case TokenType::PlusEqual:
        case TokenType::AsteriskEqual:
        case TokenType::ForwardSlashEqual:
        case TokenType::AmpersanEqual:
        case TokenType::OrEqual:
        case TokenType::DoubleGreaterThanEqual:
        case TokenType::DoubleLessThanEqual:
        case TokenType::PercentEqual:
        case TokenType::MinusMinus:
        case TokenType::PlusPlus:
        case TokenType::If:
        case TokenType::Else:
        case TokenType::Colon:
        case TokenType::QuestionMark:
        case TokenType::For:
        case TokenType::While:
        case TokenType::Do:
        case TokenType::Break:
        case TokenType::Continue:
        case TokenType::CaretEqual:
            std::unreachable();
    }
}
}  // namespace

namespace parser {

using lexer::TokenType;

std::expected<std::unique_ptr<Program>, std::string> parse_program(
    lexer::TokenStream token_stream) {
    const auto restore_state = token_stream.save();
    std::vector<std::unique_ptr<Function>> functions;

    while (token_stream.peek(0)) {
        auto function = parse_function(token_stream);
        if (!function) {
            token_stream.restore(restore_state);
            return std::unexpected("Failed to parse function: " +
                                   function.error());
        }

        functions.push_back(std::move(*function));
    }

    return std::make_unique<Program>(std::move(functions));
}

std::expected<std::unique_ptr<Function>, std::string> parse_function(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();
    if (!token_stream.consume_if(TokenType::Int)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse function: malformed return type");
    }

    const auto id_token = token_stream.try_consume(TokenType::Identifier);
    if (!id_token) {
        token_stream.restore(restore_state);

        return std::unexpected(
            "Failed to parse function: malformed identifier");
    }

    if (!token_stream.consume_if(TokenType::OpenParen)) {
        token_stream.restore(restore_state);
        return std::unexpected("Failed to parse function: missing open paren");
    }
    std::vector<std::string> params;
    const auto parse_argument = [&]() -> std::optional<std::string> {
        auto id = token_stream.try_consume(TokenType::Identifier);
        if (!id) {
            return std::nullopt;
        }
        return id->m_data;
    };

    if (token_stream.consume_if(TokenType::Int)) {
        auto first_param = parse_argument();
        if (!first_param) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse function arguments, expected identifier");
        }
        params.push_back(*first_param);

        while (token_stream.consume_if(TokenType::Comma)) {
            if (!token_stream.consume_if(TokenType::Int)) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse function argumenst, expected int");
            }
            auto param = parse_argument();
            if (!param) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse function arguments, expected identifier");
            }
            params.push_back(*param);
        }
    }

    if (!token_stream.consume_if(TokenType::CloseParen)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse function: missing closing paren");
    }

    if (token_stream.consume_if(TokenType::Semicolon)) {
        return std::make_unique<Function>(id_token->m_data, std::move(params),
                                          nullptr);
    }

    auto body = parse_compound(token_stream);
    if (!body) {
        token_stream.restore(restore_state);
        return std::unexpected("Failed to parse function: " + body.error());
    }

    return std::make_unique<Function>(id_token->m_data, std::move(params),
                                      std::move(*body));
}

std::expected<std::unique_ptr<BlockItem>, std::string> parse_block_item(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    if (auto declaration = parse_declaration(token_stream)) {
        return declaration;
    }
    if (auto statement = parse_statement(token_stream)) {
        return statement;
    }

    token_stream.restore(restore_state);
    return std::unexpected("Failed to parse block item");
}

std::expected<std::unique_ptr<Declaration>, std::string> parse_declaration(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    if (!token_stream.consume_if(TokenType::Int)) {
        token_stream.restore(restore_state);
        return std::unexpected("Failed to parse declaration");
    }

    auto id = token_stream.try_consume(TokenType::Identifier);
    if (!id) {
        token_stream.restore(restore_state);
        return std::unexpected("Failed to parse statement: missing identifier");
    }

    std::unique_ptr<Expression> initializer{nullptr};

    if (token_stream.consume_if(TokenType::Equal)) {
        auto init = parse_expression(token_stream);
        if (!init) {
            return std::unexpected("Failed to parse initialzer: " +
                                   init.error());
        }
        initializer = std::move(*init);
    }

    if (!token_stream.consume_if(TokenType::Semicolon)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse declaration: missing semicolon");
    }

    return std::make_unique<Declaration>(id->m_data, std::move(initializer));
}

std::expected<std::unique_ptr<CompoundStatement>, std::string> parse_compound(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();
    if (!token_stream.consume_if(TokenType::OpenBrace)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse compound statement: missing open brace");
    }

    std::vector<std::unique_ptr<BlockItem>> m_block_items;

    while (auto item = parse_block_item(token_stream)) {
        m_block_items.push_back(std::move(*item));
    }

    if (!token_stream.consume_if(TokenType::CloseBrace)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse compound statement: missing closing brace");
    }

    return std::make_unique<parser::CompoundStatement>(
        std::move(m_block_items));
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_optional_exression_semicolon(lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    auto expr = parse_expression(token_stream);
    if (!token_stream.consume_if(TokenType::Semicolon)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse optional expression: missing semicolon");
    }
    if (!expr) {
        return std::make_unique<EmptyExpression>();
    }
    return expr;
}
std::expected<std::unique_ptr<Expression>, std::string>
parse_optional_exression_paren(lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    auto expr = parse_expression(token_stream);
    if (!token_stream.consume_if(TokenType::CloseParen)) {
        token_stream.restore(restore_state);
        return std::unexpected(
            "Failed to parse optional expression: missing closing paren");
    }
    if (!expr) {
        return std::make_unique<EmptyExpression>();
    }
    return expr;
}

std::expected<std::unique_ptr<Statement>, std::string>
parse_statement(  // NOLINT
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    if (token_stream.consume_if(TokenType::Return)) {
        auto expr = parse_expression(token_stream);

        if (!expr) {
            token_stream.restore(restore_state);
            return std::unexpected(expr.error());
        }
        if (!token_stream.consume_if(TokenType::Semicolon)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse statement: missing semicolon");
        }

        return std::make_unique<ReturnStatement>(std::move(*expr));
    }

    if (token_stream.consume_if(TokenType::If)) {
        if (!token_stream.consume_if(TokenType::OpenParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Unable to parse if statement: expected open paren");
        }
        auto cond_expr = parse_expression(token_stream);
        if (!cond_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Unable to parse if statement: Failed to parse condition "
                "clause: " +
                cond_expr.error());
        }

        if (!token_stream.consume_if(TokenType::CloseParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Unable to parse if statement: expected close paren");
        }

        auto then_statement = parse_statement(token_stream);
        std::unique_ptr<Statement> else_statement{nullptr};

        if (token_stream.consume_if(TokenType::Else)) {
            auto maybe_else_statement = parse_statement(token_stream);
            if (!maybe_else_statement) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Unable to parse if statement: unable to parse else "
                    "clause: " +
                    maybe_else_statement.error());
            }
            else_statement = std::move(*maybe_else_statement);
        }
        return std::make_unique<IfStatement>(std::move(*cond_expr),
                                             std::move(*then_statement),
                                             std::move(else_statement));
    }

    if (token_stream.consume_if(TokenType::For)) {
        if (!token_stream.consume_if(TokenType::OpenParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Unable to parse for loop: missing open paren");
        }

        if (auto initial_expr =
                parse_optional_exression_semicolon(token_stream)) {
            auto control_expr =
                parse_optional_exression_semicolon(token_stream);
            if (!control_expr) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse for loop: missing control expression");
            }
            // for (;;) gets transformed in AST to for (;1;)
            if (control_expr.value()->m_expr_type ==
                parser::ExpressionType::Empty) {
                control_expr =
                    std::make_unique<parser::IntLiteralExpression>(1);
            }

            auto post_expr = parse_optional_exression_paren(token_stream);
            if (!post_expr) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse for loop: mising post expression");
            }

            auto inner_statement = parse_statement(token_stream);
            if (!inner_statement) {
                token_stream.restore(restore_state);
                return std::unexpected(inner_statement.error());
            }

            return std::make_unique<ForStatement>(
                std::move(*initial_expr), std::move(*control_expr),
                std::move(*post_expr), std::move(*inner_statement));
        }

        if (auto decl = parse_declaration(token_stream)) {
            auto control_expr =
                parse_optional_exression_semicolon(token_stream);
            if (!control_expr) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse for loop: missing control expression");
            }

            auto post_expr = parse_optional_exression_paren(token_stream);
            if (!post_expr) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse for loop: mising post expression");
            }

            auto inner_statement = parse_statement(token_stream);
            if (!inner_statement) {
                token_stream.restore(restore_state);
                return std::unexpected(inner_statement.error());
            }

            return std::make_unique<ForDeclStatement>(
                std::move(*decl), std::move(*control_expr),
                std::move(*post_expr), std::move(*inner_statement));
        }
    }
    if (token_stream.consume_if(TokenType::While)) {
        if (!token_stream.consume_if(TokenType::OpenParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse while loop: missing open paren");
        }

        auto cond_expr = parse_expression(token_stream);
        if (!cond_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(cond_expr.error());
        }

        if (!token_stream.consume_if(TokenType::CloseParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse while loop: missing close paren");
        }

        auto statement = parse_statement(token_stream);
        if (!statement) {
            token_stream.restore(restore_state);
            return std::unexpected(statement.error());
        }

        return std::make_unique<WhileStatement>(std::move(*cond_expr),
                                                std::move(*statement));
    }
    if (token_stream.consume_if(TokenType::Do)) {
        auto statement = parse_statement(token_stream);
        if (!statement) {
            token_stream.restore(restore_state);
            return std::unexpected(statement.error());
        }

        if (!token_stream.consume_if(TokenType::While)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse do while loop: missing while");
        }

        if (!token_stream.consume_if(TokenType::OpenParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse do while loop: missing open paren");
        }

        auto cond_expr = parse_expression(token_stream);
        if (!cond_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(cond_expr.error());
        }

        if (!token_stream.consume_if(TokenType::CloseParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse do while loop: missing close paren");
        }
        if (!token_stream.consume_if(TokenType::Semicolon)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse do while loop: missing semicolon");
        }

        return std::make_unique<DoStatement>(std::move(*cond_expr),
                                             std::move(*statement));
    }
    if (token_stream.consume_if(TokenType::Break)) {
        if (!token_stream.consume_if(TokenType::Semicolon)) {
            token_stream.restore(restore_state);
            return std::unexpected("Failed to parse break: Missing semicolon");
        }
        return std::make_unique<BreakStatement>();
    }
    if (token_stream.consume_if(TokenType::Continue)) {
        if (!token_stream.consume_if(TokenType::Semicolon)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse continue: Missing semicolon");
        }
        return std::make_unique<ContinueStatement>();
    }

    if (auto compound = parse_compound(token_stream)) {
        return compound;
    }

    if (auto expr = parse_optional_exression_semicolon(token_stream)) {
        return std::make_unique<ExpressionStatement>(std::move(*expr));
    }

    token_stream.restore(restore_state);
    return std::unexpected("Failed to parse statement");
}

template <auto child_parse, lexer::TokenType... Tokens>
std::expected<std::unique_ptr<Expression>, std::string> parse_expression_t(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();
    auto maybe_expr = child_parse(token_stream);
    if (!maybe_expr) {
        token_stream.restore(restore_state);
        return std::unexpected(maybe_expr.error());
    }
    std::unique_ptr<Expression> expr = std::move(maybe_expr.value());

    while ((token_stream.has_tok(Tokens) || ...)) {
        const auto op_token = token_stream.consume();

        auto next_expr = child_parse(token_stream);
        if (!next_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(next_expr.error());
        }
        expr = std::make_unique<BinaryOpExpression>(
            std::move(expr), std::move(*next_expr),
            bin_op_type(op_token.m_token_type));
    }

    return expr;
}

std::expected<std::unique_ptr<Expression>, std::string> parse_expression(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_assignment_expression, TokenType::Comma>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_assignment_expression(lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    if (const auto id = token_stream.try_consume(TokenType::Identifier)) {
        if (token_stream.consume_if(TokenType::Equal)) {
            auto expr = parse_ternary_expr(token_stream);
            if (!expr) {
                token_stream.restore(restore_state);
                return std::unexpected(expr.error());
            }
            return std::make_unique<AssignmentExpression>(id->m_data,
                                                          std::move(*expr));
        }

        if (const auto op_type =
                token_stream.peek(0).and_then([](const auto& x) {
                    return compound_assign_type(x.m_token_type);
                })) {
            token_stream.consume();
            auto expr = parse_ternary_expr(token_stream);
            if (!expr) {
                token_stream.restore(restore_state);
                return std::unexpected(expr.error());
            }

            return std::make_unique<CompoundAssignmentExpression>(
                id->m_data, std::move(*expr), *op_type);
        }
    }

    token_stream.restore(restore_state);

    return parse_ternary_expr(token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_ternary_expr(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    auto cond_expression = parse_logical_or_expr(token_stream);
    if (!cond_expression) {
        token_stream.restore(restore_state);
        return std::unexpected(cond_expression.error());
    }

    if (token_stream.consume_if(TokenType::QuestionMark)) {
        auto then_exp = parse_expression(token_stream);
        if (!then_exp) {
            token_stream.restore(restore_state);
            return std::unexpected(then_exp.error());
        }
        if (!token_stream.consume_if(TokenType::Colon)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse ternary expression: missing colon");
        }
        auto else_expr = parse_ternary_expr(token_stream);
        if (!else_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse ternary expression else clause: " +
                else_expr.error());
        }

        return std::make_unique<parser::TernaryExpression>(
            std::move(*cond_expression), std::move(*then_exp),
            std::move(*else_expr));
    }

    return cond_expression;
}

std::expected<std::unique_ptr<Expression>, std::string> parse_logical_or_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_logical_and_expr, TokenType::DoubleOr>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_logical_and_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_bitwise_or_expr, TokenType::DoubleAmpersan>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_or_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_bitwise_xor_expr, TokenType::Or>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_xor_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_bitwise_and_expr, TokenType::Caret>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_bitwise_and_expr(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_equality_expression, TokenType::Ampersan>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_equality_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_relational_expression, TokenType::NotEqual,
                              TokenType::DoubleEqual>(token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_relational_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<
        parse_bitwise_shift_expr, TokenType::LessThan, TokenType::GreaterThan,
        TokenType::LessThanOrEqual, TokenType::GreaterThanOrEqual>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_bitwise_shift_expr(lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_additive_expression,
                              TokenType::DoubleLessThan,
                              TokenType::DoubleGreaterThan>(token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_additive_expression(lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_term, TokenType::Plus, TokenType::Minus>(
        token_stream);
}
std::expected<std::unique_ptr<Expression>, std::string> parse_term(
    lexer::TokenStream& token_stream) {
    return parse_expression_t<parse_unary_expression, TokenType::Asterisk,
                              TokenType::ForwardSlash, TokenType::Percent>(
        token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string> parse_unary_expression(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    const auto handle_unary_op = [&](UnaryOpType op_type)
        -> std::expected<std::unique_ptr<UnaryOpExpression>, std::string> {
        auto child_expr = parse_unary_expression(token_stream);
        if (!child_expr) {
            token_stream.restore(restore_state);
            return std::unexpected(child_expr.error());
        }
        return std::make_unique<UnaryOpExpression>(
            std::move(child_expr.value()), op_type);
    };

    if (const auto token = token_stream.try_consume(TokenType::Tilde)) {
        return handle_unary_op(UnaryOpType::BitwiseNot);
    }
    if (const auto token = token_stream.try_consume(TokenType::Bang)) {
        return handle_unary_op(UnaryOpType::LogicalNot);
    }
    if (const auto token = token_stream.try_consume(TokenType::Minus)) {
        return handle_unary_op(UnaryOpType::Negate);
    }
    if (const auto token = token_stream.try_consume(TokenType::PlusPlus)) {
        return handle_unary_op(UnaryOpType::PreIncrement);
    }
    if (const auto token = token_stream.try_consume(TokenType::MinusMinus)) {
        return handle_unary_op(UnaryOpType::PreDecrement);
    }

    return parse_postfix_expression(token_stream);
}

std::expected<std::unique_ptr<Expression>, std::string>
parse_postfix_expression(lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();
    auto maybe_expr = parse_factor(token_stream);
    if (!maybe_expr) {
        token_stream.restore(restore_state);
        return std::unexpected(maybe_expr.error());
    }
    std::unique_ptr<Expression> expr = std::move(maybe_expr.value());

    if (token_stream.has_tok(TokenType::PlusPlus) ||
        token_stream.has_tok(TokenType::MinusMinus)) {
        if (expr->m_expr_type != parser::ExpressionType::VariableRef) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Post increment can only be applied to variable");
        }

        const auto op = token_stream.consume();
        const auto op_type = op.m_token_type == TokenType::PlusPlus
                                 ? UnaryOpType::PostIncrement
                                 : UnaryOpType::PostDecrement;
        return std::make_unique<UnaryOpExpression>(std::move(expr), op_type);
    }

    return expr;
}

std::expected<std::unique_ptr<Expression>, std::string> parse_factor(
    lexer::TokenStream& token_stream) {
    const auto restore_state = token_stream.save();

    if (const auto token = token_stream.try_consume(TokenType::IntLiteral)) {
        int constant{};
        std::from_chars(token->m_data.data(),
                        token->m_data.data() + token->m_data.size(), constant);

        return std::make_unique<IntLiteralExpression>(constant);
    }

    if (const auto token = token_stream.try_consume(TokenType::OpenParen)) {
        auto child_expr = parse_expression(token_stream);
        if (!child_expr) {
            return std::unexpected(child_expr.error());
        }
        if (!token_stream.try_consume(TokenType::CloseParen)) {
            token_stream.restore(restore_state);
            return std::unexpected(
                "Failed to parse factor: expected closing paren");
        }
        return child_expr;
    }

    if (const auto token = token_stream.try_consume(TokenType::Identifier)) {
        if (token_stream.consume_if(TokenType::OpenParen)) {
            if (token_stream.consume_if(TokenType::CloseParen)) {
                return std::make_unique<FunctionCallExpression>(
                    token->m_data, std::vector<std::unique_ptr<Expression>>{});
            }
            std::vector<std::unique_ptr<Expression>> arguments;

            auto first_arg = parse_assignment_expression(token_stream);
            if (!first_arg) {
                token_stream.restore(restore_state);
                return std::unexpected("Failed to parse function call: " +
                                       first_arg.error());
            }
            arguments.push_back(std::move(*first_arg));

            while (token_stream.consume_if(TokenType::Comma)) {
                auto arg = parse_assignment_expression(token_stream);
                if (!arg) {
                    token_stream.restore(restore_state);
                    return std::unexpected("Failed to parse function call: " +
                                           arg.error());
                }
                arguments.push_back(std::move(*arg));
            }

            if (!token_stream.consume_if(TokenType::CloseParen)) {
                token_stream.restore(restore_state);
                return std::unexpected(
                    "Failed to parse function call: expected closing paren");
            }

            return std::make_unique<FunctionCallExpression>(
                token->m_data, std::move(arguments));
        }
        return std::make_unique<VariableRefExpression>(token->m_data);
    }

    token_stream.restore(restore_state);
    return std::unexpected(
        fmt::format("Failed to parse factor: expected integer literal or "
                    "unary operator, found: {}",
                    token_stream.peek()
                        .transform([](const auto& s) { return s.m_data; })
                        .value_or("EOF")));
}

[[nodiscard]] std::string ForStatement::to_string(int indent) const {
    std::string result = fmt::format("{}ForStatement\n", get_indent(indent));
    result += fmt::format("{}Initial:\n{}", get_indent(indent + 1),
                          m_initial->to_string(indent + 2));
    result += fmt::format("{}Control:\n{}", get_indent(indent + 1),
                          m_control->to_string(indent + 2));
    result += fmt::format("{}Post:\n{}", get_indent(indent + 1),
                          m_post->to_string(indent + 2));
    result += fmt::format("{}Body:\n{}", get_indent(indent + 1),
                          m_statement->to_string(indent + 2));
    return result;
}

[[nodiscard]] std::string ForDeclStatement::to_string(int indent) const {
    std::string result =
        fmt::format("{}ForDeclStatement\n", get_indent(indent));
    result += fmt::format("{}Initial:\n{}", get_indent(indent + 1),
                          m_initial->to_string(indent + 2));
    result += fmt::format("{}Control:\n{}", get_indent(indent + 1),
                          m_control->to_string(indent + 2));
    result += fmt::format("{}Post:\n{}", get_indent(indent + 1),
                          m_post->to_string(indent + 2));
    result += fmt::format("{}Body:\n{}", get_indent(indent + 1),
                          m_statement->to_string(indent + 2));
    return result;
}
[[nodiscard]] std::string WhileStatement::to_string(int indent) const {
    std::string result = fmt::format("{}WhileStatement\n", get_indent(indent));
    result += fmt::format("{}Condition:\n{}", get_indent(indent + 1),
                          m_cond_expr->to_string(indent + 2));
    result += fmt::format("{}Body:\n{}", get_indent(indent + 1),
                          m_statement->to_string(indent + 2));
    return result;
}

[[nodiscard]] std::string DoStatement::to_string(int indent) const {
    std::string result = fmt::format("{}DoStatement\n", get_indent(indent));
    result += fmt::format("{}Body:\n{}", get_indent(indent + 1),
                          m_statement->to_string(indent + 2));
    result += fmt::format("{}Condition:\n{}", get_indent(indent + 1),
                          m_cond_expr->to_string(indent + 2));
    return result;
}

[[nodiscard]] std::string BreakStatement::to_string(int indent) const {
    std::string result = fmt::format("{}BreakStatement\n", get_indent(indent));
    return result;
}
[[nodiscard]] std::string ContinueStatement::to_string(int indent) const {
    std::string result =
        fmt::format("{}ContinueStatement\n", get_indent(indent));
    return result;
}

[[nodiscard]] std::string BinaryOpExpression::to_string(int indent) const {
    using namespace std::literals;
    const auto op_str = [&]() {
        switch (m_op_type) {
            case BinaryOpType::Add:
                return "+"sv;
            case BinaryOpType::Subtract:
                return "-"sv;
            case BinaryOpType::Multiply:
                return "*"sv;
            case BinaryOpType::Divide:
                return "/"sv;
            case BinaryOpType::LessThan:
                return "<"sv;
            case BinaryOpType::GreaterThan:
                return ">"sv;
            case BinaryOpType::LessThanOrEqual:
                return "<="sv;
            case BinaryOpType::GreaterThanOrEqual:
                return ">="sv;
            case BinaryOpType::Equal:
                return "=="sv;
            case BinaryOpType::NotEqual:
                return "!="sv;
            case BinaryOpType::BitwiseAnd:
                return "&"sv;
            case BinaryOpType::LogicalAnd:
                return "&&"sv;
            case BinaryOpType::BitwiseOr:
                return "|"sv;
            case BinaryOpType::LogicalOr:
                return "||"sv;
            case BinaryOpType::Modulo:
                return "%"sv;
            case BinaryOpType::BitwiseXor:
                return "^"sv;
            case BinaryOpType::RightShift:
                return ">>"sv;
            case BinaryOpType::LeftShift:
                return "<<"sv;
            case BinaryOpType::Comma:
                return ","sv;
        }
    }();

    return fmt::format("{}BinaryOpExpr: {}\n{}Left:\n{}\n{}Right:\n{}",
                       get_indent(indent), op_str, get_indent(indent + 1),
                       m_lhs->to_string(indent + 2), get_indent(indent + 1),
                       m_rhs->to_string(indent + 2));
}

[[nodiscard]] std::string CompoundStatement::to_string(int indent) const {
    std::string result =
        fmt::format("{}CompoundStatement\n", get_indent(indent));
    for (const auto& item : m_block_items) {
        result += item->to_string(indent + 1);
        result += "\n";
    }
    return result;
}

[[nodiscard]] std::string EmptyExpression::to_string(int indent) const {
    return fmt::format("{}EmptyExpression\n", get_indent(indent));
}

[[nodiscard]] std::string CompoundAssignmentExpression::to_string(
    int indent) const {
    using namespace std::literals;
    const auto op_str = [&]() {
        switch (m_op_type) {
            case CompoundAssignmentType::PlusEqual:
                return "+="sv;
            case CompoundAssignmentType::MinusEqual:
                return "-="sv;
            case CompoundAssignmentType::TimesEqual:
                return "*="sv;
            case CompoundAssignmentType::DivideEqual:
                return "/="sv;
            case CompoundAssignmentType::ModuloEqual:
                return "%="sv;
            case CompoundAssignmentType::LeftShiftEqual:
                return "<<="sv;
            case CompoundAssignmentType::RightShiftEqual:
                return ">>="sv;
            case CompoundAssignmentType::BitwiseAndEqual:
                return "&="sv;
            case CompoundAssignmentType::BitwiseOrEqual:
                return "|="sv;
            case CompoundAssignmentType::XorEqual:
                return "^="sv;
        }
    }();

    return fmt::format("{}CompoundExpr: {}\n{}Variable: {}\n{}Expression:\n{}",
                       get_indent(indent), op_str, get_indent(indent + 1),
                       m_var_name, get_indent(indent + 1),
                       m_expr->to_string(indent + 2));
}

[[nodiscard]] std::string FunctionCallExpression::to_string(int indent) const {
    std::string result =
        fmt::format("{}FunctionCall: {}", get_indent(indent), m_func_name);
    for (const auto& arg : m_arguments) {
        result += "\n" + fmt::format("{}Argument:\n{}", get_indent(indent + 1),
                                     arg->to_string(indent + 2));
    }
    return result;
}

[[nodiscard]] std::string IfStatement::to_string(int indent) const {
    std::string result = fmt::format("{}IfStatement\n", get_indent(indent));
    result += fmt::format("{}Condition:\n{}", get_indent(indent + 1),
                          m_condition->to_string(indent + 2));
    result += fmt::format("{}Then:\n{}", get_indent(indent + 2),
                          m_then->to_string(indent + 2));
    if (m_else) {
        result += fmt::format("{}Else:\n{}", get_indent(indent + 1),
                              m_else->to_string(indent + 2));
    }
    return result;
}

[[nodiscard]] std::string IntLiteralExpression::to_string(int indent) const {
    return fmt::format("{}IntLiteral: {}", get_indent(indent), m_literal);
}

[[nodiscard]] std::string UnaryOpExpression::to_string(int indent) const {
    const std::string op = [&]() {
        switch (m_op_type) {
            case UnaryOpType::LogicalNot:
                return "!";
            case UnaryOpType::BitwiseNot:
                return "~";
            case UnaryOpType::Negate:
                return "-";
            case UnaryOpType::PreIncrement:
            case UnaryOpType::PostIncrement:
                return "++";
            case UnaryOpType::PreDecrement:
            case UnaryOpType::PostDecrement:
                return "--";
        }
    }();

    return fmt::format("{}UnaryOp: {}\n{}", get_indent(indent), op,
                       m_expr->to_string(indent + 1));
}
[[nodiscard]] std::string AssignmentExpression::to_string(int indent) const {
    return fmt::format("{}AssignmentExpr\n{}LHS:\n{}\n{}RHS:\n{}",
                       get_indent(indent), get_indent(indent + 1), m_var_name,
                       get_indent(indent + 1), m_expr->to_string(indent + 2));
}

[[nodiscard]] std::string VariableRefExpression::to_string(int indent) const {
    return fmt::format("{}VariableRef: {}", get_indent(indent), m_var_name);
}

[[nodiscard]] std::string TernaryExpression::to_string(int indent) const {
    return fmt::format(
        "{}TernaryExpr\n{}Condition:\n{}\n{}Then:\n{}\n{}Else:\n{}",
        get_indent(indent), get_indent(indent + 1),
        m_cond->to_string(indent + 2), get_indent(indent + 1),
        m_then->to_string(indent + 2), get_indent(indent + 1),
        m_else->to_string(indent + 2));
}

[[nodiscard]] std::string ExpressionStatement::to_string(int indent) const {
    if (m_expr) {
        return fmt::format("{}ExpressionStmt:\n{}", get_indent(indent),
                           m_expr->to_string(indent + 1));
    }
    return fmt::format("{}ExpressionStmt:\nEmpty", get_indent(indent));
}

[[nodiscard]] std::string Declaration::to_string(int indent) const {
    if (m_initializer) {
        return fmt::format("{}Declaration: {}\n{}Initializer:\n{}",
                           get_indent(indent), m_var_name,
                           get_indent(indent + 1),
                           m_initializer->to_string(indent + 2));
    }
    return fmt::format("{}DeclarationStmt: {}", get_indent(indent), m_var_name);
}

[[nodiscard]] std::string ReturnStatement::to_string(int indent) const {
    return fmt::format("{}Return\n{}", get_indent(indent),
                       m_expr->to_string(indent + 1));
}

[[nodiscard]] std::string Function::to_string(int indent) const {
    std::string result =
        fmt::format("{}Function: {}", get_indent(indent), m_name);

    if (!m_params.empty()) {
        result += " (";
        for (size_t i = 0; i < m_params.size(); ++i) {
            result += m_params[i];
            if (i < m_params.size() - 1) {
                result += ", ";
            }
        }
        result += ")";
    }

    if (m_body) {
        for (const auto& item : m_body->m_block_items) {
            result += "\n" + item->to_string(indent + 1);
        }
    }
    return result;
}

[[nodiscard]] std::string Program::to_string(int indent) const {
    std::string result = fmt::format("{}Program", get_indent(indent));
    for (const auto& func : m_functions) {
        result += "\n" + func->to_string(indent + 1);
    }
    return result;
}
// [[nodiscard]] std::expected<FactorV, std::string> parse_factor_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//     auto token = *tokens;
//     ++tokens;
//
//     switch (token.m_token_type) {
//         case TokenType::IntLiteral: {
//             int constant{};
//             std::from_chars(token.m_data.data(),
//                             token.m_data.data() + token.m_data.size(),
//                             constant);
//
//             return std::make_unique<IntLiteralFactorV>(constant);
//         }
//         case TokenType::Tilde:
//         case TokenType::Bang:
//         case TokenType::Dash: {
//             auto child_expr = parse_factor_v(tokens);
//             if (!child_expr) {
//                 return std::unexpected(child_expr.error());
//             }
//
//             const auto type = token.m_token_type == TokenType::Tilde
//                                   ? UnaryOpType::BitwiseNot
//                               : token.m_token_type == TokenType::Bang
//                                   ? UnaryOpType::LogicalNot
//                                   : UnaryOpType::Negate;
//
//             return std::make_unique<UnaryOpFactorV>(
//                 std::move(child_expr.value()), type);
//         }
//         case TokenType::OpenParen: {
//             auto child_expr = parse_expression_v(tokens);
//             if (!child_expr) {
//                 return std::unexpected(child_expr.error());
//             }
//             if (tokens->m_token_type != TokenType::CloseParen) {
//                 return std::unexpected(
//                     "Failed to parse factor: expected closing paren");
//             }
//             ++tokens;
//             return std::make_unique<ParenGroupFactorV>(
//                 std::move(child_expr.value()));
//         }
//         default:
//             return std::unexpected(fmt::format(
//                 "Failed to parse factor: expected integer literal or "
//                 "unary operator, received: {}",
//                 token.m_data));
//     }
// }
//
// [[nodiscard]] std::expected<TermV, std::string> parse_term_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//
//     auto maybe_factor = parse_factor_v(tokens);
//     if (!maybe_factor) {
//         return std::unexpected(maybe_factor.error());
//     }
//
//     TermV factor = std::move(maybe_factor.value());
//
//     while (tokens->m_token_type == TokenType::Asterisk ||
//            tokens->m_token_type == TokenType::ForwardSlash) {
//         auto op_type = tokens->m_token_type == TokenType::Asterisk
//                            ? BinaryOpType::Multiply
//                            : BinaryOpType::Divide;
//         ++tokens;
//
//         auto next_factor = parse_factor_v(tokens);
//         if (!next_factor) {
//             return std::unexpected(next_factor.error());
//         }
//
//         factor = std::make_unique<BinaryOpExpressionV>(
//             std::move(factor), std::move(next_factor.value()), op_type);
//     }
//
//     return factor;
// }
//
// [[nodiscard]] std::expected<ExpressionV, std::string> parse_expression_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     using lexer::TokenType;
//
//     auto maybe_term = parse_term_v(tokens);
//     if (!maybe_term) {
//         return std::unexpected(maybe_term.error());
//     }
//     ExpressionV term = std::move(maybe_term.value());
//
//     while (tokens->m_token_type == TokenType::Plus ||
//            tokens->m_token_type == TokenType::Dash) {
//         auto op_type = tokens->m_token_type == TokenType::Plus
//                            ? BinaryOpType::Add
//                            : BinaryOpType::Subtract;
//         ++tokens;
//
//         auto next_term = parse_term_v(tokens);
//         if (!next_term) {
//             return std::unexpected(next_term.error());
//         }
//         term = std::make_unique<BinaryOpExpressionV>(
//             std::move(term), std::move(next_term.value()), op_type);
//     }
//
//     return term;
// }
//
// [[nodiscard]] std::expected<StatementV, std::string> parse_statement_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     if (tokens->m_token_type != lexer::TokenType::Return) {
//         return std::unexpected(
//             "Failed to parse statement: missing return keyword");
//     }
//     ++tokens;
//     auto expr = parse_expression_v(tokens);
//     if (!expr) {
//         return std::unexpected(expr.error());
//     }
//
//     if (tokens->m_token_type != lexer::TokenType::Semicolon) {
//         return std::unexpected("Failed to parse statement: missing
//         semicolon");
//     }
//     ++tokens;
//
//     return std::make_unique<ReturnStatementV>(std::move(expr.value()));
// }
//
// [[nodiscard]] std::expected<P<FunctionV>, std::string> parse_function_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     if (tokens->m_token_type != lexer::TokenType::Int) {
//         return std::unexpected(
//             "Failed to parse function: malformed return type");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::Identifier) {
//         return std::unexpected("Failed to parse function: malformed
//         name");
//     }
//     auto name = tokens->m_data;
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::OpenParen) {
//         return std::unexpected("Failed to parse function: missing open
//         paren");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::CloseParen) {
//         return std::unexpected(
//             "Failed to parse function: missing closing paren");
//     }
//     ++tokens;
//
//     if (tokens->m_token_type != lexer::TokenType::OpenBrace) {
//         return std::unexpected("Failed to parse function: missing open
//         brace");
//     }
//     ++tokens;
//
//     auto statement = parse_statement_v(tokens);
//
//     if (!statement) {
//         return std::unexpected(statement.error());
//     }
//
//     if (tokens->m_token_type != lexer::TokenType::CloseBrace) {
//         return std::unexpected(
//             "Failed to parse function: missing closing brace");
//     }
//     ++tokens;
//
//     return std::make_unique<FunctionV>(name,
//     std::move(statement.value()));
// }
//
// [[nodiscard]] std::expected<P<ProgramV>, std::string> parse_program_v(
//     std::vector<lexer::Token>::iterator& tokens) {
//     auto function = parse_function_v(tokens);
//     if (!function) {
//         return std::unexpected(function.error());
//     }
//
//     return std::make_unique<ProgramV>(std::move(function.value()));
// }
//
// std::string ast_to_string(AstNodeV node, int indent) {
//     struct Visit {
//         int indent;
//         std::string operator()(P<ProgramV>& n) const {
//             return fmt::format(
//                 "{}Program\n{}", get_indent(indent),
//                 ast_to_string(std::move(n->m_function), indent + 1));
//         }
//         std::string operator()(P<FunctionV>& n) const {
//             return fmt::format(
//                 "{}Function: {}\n{}", get_indent(indent), n->m_name,
//                 ast_to_string(std::move(n->m_statement), indent + 1));
//         }
//         std::string operator()(StatementV& n) const {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p),
//                 indent);
//                 }, n);
//         }
//         std::string operator()(P<ReturnStatementV>& n) const {
//             return fmt::format("{}Return\n{}", get_indent(indent),
//                                ast_to_string(std::move(n->m_expr), indent
//                                + 1));
//         }
//         std::string operator()(ExpressionV& n) const {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p),
//                 indent);
//                 }, n);
//         }
//         std::string operator()(TermV& n) {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p),
//                 indent);
//                 }, n);
//         }
//         std::string operator()(FactorV& n) {
//             return std::visit(
//                 [this](auto& p) { return ast_to_string(std::move(p),
//                 indent);
//                 }, n);
//         }
//         std::string operator()(P<BinaryOpExpressionV>& n) const {
//             const auto op_char = [&]() {
//                 switch (n->m_op_type) {
//                     case BinaryOpType::Add:
//                         return '+';
//                     case BinaryOpType::Subtract:
//                         return '-';
//                     case BinaryOpType::Multiply:
//                         return '*';
//                     case BinaryOpType::Divide:
//                         return '/';
//                     default:
//                         std::unreachable();
//                 }
//             }();
//
//             return fmt::format("{}BinaryOp:
//             {}\n{}Left:\n{}\n{}Right:\n{}",
//                                get_indent(indent), op_char,
//                                get_indent(indent + 1),
//                                ast_to_string(std::move(n->m_lhs), indent
//                                + 2), get_indent(indent + 1),
//                                ast_to_string(std::move(n->m_rhs), indent
//                                + 2));
//         }
//         std::string operator()(P<ParenGroupFactorV>& n) const {
//             return fmt::format(
//                 "{}ParenGroup\n{}", get_indent(indent),
//                 ast_to_string(std::move(n->m_expression), indent + 1));
//         }
//         std::string operator()(P<UnaryOpFactorV>& n) const {
//             const auto op = [&]() {
//                 switch (n->m_op_type) {
//                     case UnaryOpType::LogicalNot:
//                         return '!';
//                     case UnaryOpType::BitwiseNot:
//                         return '~';
//                     case UnaryOpType::Negate:
//                         return '-';
//                 }
//             }();
//
//             return fmt::format(
//                 "{}UnaryOp: {}\n{}", get_indent(indent), op,
//                 ast_to_string(std::move(n->m_factor), indent + 1));
//         }
//         std::string operator()(P<IntLiteralFactorV>& n) const {
//             return fmt::format("{}IntLiteral: {}", get_indent(indent),
//                                n->m_literal);
//         }
//     };
//
//     return std::visit(Visit{indent}, node);
// }

}  // namespace parser
