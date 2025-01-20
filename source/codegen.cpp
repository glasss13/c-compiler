#include "codegen.hpp"

#include <iostream>

#include "parser.hpp"

namespace codegen {

[[nodiscard]] std::string codegen_program(const parser::Program& program) {
    return codegen_function(*program.m_function);
}

[[nodiscard]] std::string codegen_function(const parser::Function& function) {
    return fmt::format(".globl _{}\n_{}:\n{}", function.m_name, function.m_name,
                       codegen_statement(*function.m_statement));
}
[[nodiscard]] std::string codegen_statement(
    const parser::Statement& statement) {
    switch (statement.m_statement_type) {
        case parser::StatmentType::Return:
            const auto& ret_statement =
                dynamic_cast<const parser::ReturnStatement&>(statement);

            return fmt::format("{}\nret",
                               codegen_expression(*ret_statement.m_expr));
    }
}

[[nodiscard]] std::string codegen_expression(const parser::Expression& expr) {
    switch (expr.m_expr_type) {
        case parser::ExpressionType::BinaryOp:
            return codegen_binary_op(
                dynamic_cast<const parser::BinaryOpExpression&>(expr));
        case parser::ExpressionType::UnaryOp: {
            const auto& unary_op_expr =
                dynamic_cast<const parser::UnaryOpExpression&>(expr);
            return codegen_unary_op(unary_op_expr);
        }
        case parser::ExpressionType::IntLiteral: {
            const auto& literal_expr =
                dynamic_cast<const parser::IntLiteralExpression&>(expr);
            return fmt::format("mov w0, #{}", literal_expr.m_literal);
        }
    }
}

[[nodiscard]] std::string codegen_binary_op(
    const parser::BinaryOpExpression& expr) {
    const auto lhs_gen = codegen_expression(*expr.m_lhs);
    const auto rhs_gen = codegen_expression(*expr.m_rhs);
    switch (expr.m_op_type) {
        case parser::BinaryOpType::Add: {
            return fmt::format(
                "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nadd w0, "
                "w0, w1",
                lhs_gen, rhs_gen);
        }
        case parser::BinaryOpType::Multiply: {
            return fmt::format(
                "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nmul w0, "
                "w0, w1",
                lhs_gen, rhs_gen);
        }
        case parser::BinaryOpType::Subtract:
            std::cerr << "generating subtract\n";
            return fmt::format(
                "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nsub w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::Divide:
            return fmt::format(
                "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nsdiv w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
    }
}

[[nodiscard]] std::string codegen_unary_op(
    const parser::UnaryOpExpression& unary_op) {
    switch (unary_op.m_op_type) {
        case parser::UnaryOpType::LogicalNot:
            return fmt::format("{}\ncmp w0, 0\ncset w0, eq",
                               codegen_expression(*unary_op.m_expr));
        case parser::UnaryOpType::BitwiseNot:
            return fmt::format("{}\nmvn w0, w0",
                               codegen_expression(*unary_op.m_expr));
        case parser::UnaryOpType::Negate:
            return fmt::format("{}\nneg w0, w0",
                               codegen_expression(*unary_op.m_expr));
    }
}

}  // namespace codegen
