#include "codegen.hpp"

#include <iostream>

#include "parser.hpp"

namespace {
std::string label(int label_idx) { return fmt::format("L{}", label_idx); }
}  // namespace

namespace codegen {

[[nodiscard]] std::string AArch64Generator::codegen_program(
    const parser::Program& program) const {
    return codegen_function(*program.m_function);
}

[[nodiscard]] std::string AArch64Generator::codegen_function(
    const parser::Function& function) const {
    return fmt::format(".globl _{}\n_{}:\n{}", function.m_name, function.m_name,
                       codegen_statement(*function.m_statement));
}
[[nodiscard]] std::string AArch64Generator::codegen_statement(
    const parser::Statement& statement) const {
    switch (statement.m_statement_type) {
        case parser::StatmentType::Return:
            const auto& ret_statement =
                dynamic_cast<const parser::ReturnStatement&>(statement);

            return fmt::format("{}\nret",
                               codegen_expression(*ret_statement.m_expr));
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_expression(
    const parser::Expression& expr) const {
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

[[nodiscard]] std::string AArch64Generator::codegen_binary_op(
    const parser::BinaryOpExpression& expr) const {
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
        case parser::BinaryOpType::Equal:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, eq\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::LessThan:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, lt\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::GreaterThan:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, gt\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::LessThanOrEqual:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, le\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::GreaterThanOrEqual:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, ge\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::NotEqual:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "cmp w1, w0\n"
                "cset w0, ne\n",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::LogicalAnd: {
            const auto clause2_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            return fmt::format(
                "{}\n"
                "cbnz w0, {}\n"
                "b {}\n"
                "{}:\n"
                "{}\n"
                "cmp w0, 0\n"
                "cset w0, ne\n"
                "{}:\n",
                lhs_gen, clause2_label, end_label, clause2_label, rhs_gen,
                end_label);
        }

        case parser::BinaryOpType::LogicalOr: {
            const auto clause2_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            return fmt::format(
                "{}\n"
                "cbz w0, {}\n"
                "mov w0, #1\n"
                "b {}\n"
                "{}:\n"
                "{}\n"
                "cmp w0, 0\n"
                "cset w0, ne\n"
                "{}:\n",
                lhs_gen, clause2_label, end_label, clause2_label, rhs_gen,
                end_label);
        }
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_unary_op(
    const parser::UnaryOpExpression& unary_op) const {
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
