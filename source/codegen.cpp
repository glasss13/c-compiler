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
    if (const auto* binary_op_expr =
            dynamic_cast<const parser::BinaryOp*>(&expr)) {
        switch (binary_op_expr->m_op_type) {
            case parser::BinaryOpType::Add: {
                auto lhs_gen = codegen_expression(*binary_op_expr->m_lhs);
                auto rhs_gen = codegen_expression(*binary_op_expr->m_rhs);
                return fmt::format(
                    "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nadd w0, "
                    "w0, w1",
                    lhs_gen, rhs_gen);
            }

            case parser::BinaryOpType::Multiply: {
                auto lhs_gen = codegen_expression(*binary_op_expr->m_lhs);
                auto rhs_gen = codegen_expression(*binary_op_expr->m_rhs);
                return fmt::format(
                    "{}\nstr w0, [sp, #-16]!\n{}\nldr w1, [sp], #16\nmul w0, "
                    "w0, w1",
                    lhs_gen, rhs_gen);
            }
                // TODO
            case parser::BinaryOpType::Subtract:
                std::cout << "subtract\n";
                break;
            case parser::BinaryOpType::Divide:
                std::cout << "divide\n";
                std::unreachable();
        }
    } else if (const auto* term_expr =
                   dynamic_cast<const parser::TermExpression*>(&expr)) {
        return codegen_expression(*term_expr->m_term);
    } else if (const auto* factor_term =
                   dynamic_cast<const parser::FactorTerm*>(&expr)) {
        return codegen_expression(*factor_term->m_factor);
    } else if (const auto* factor =
                   dynamic_cast<const parser::Factor*>(&expr)) {
        switch (factor->m_factor_type) {
            case parser::FactorType::Constant: {
                const auto& const_expression =
                    dynamic_cast<const parser::IntLiteralFactor&>(*factor);

                return fmt::format("mov w0, #{}", const_expression.m_literal);
            }
            case parser::FactorType::UnaryOp: {
                const auto& unary_op_expression =
                    dynamic_cast<const parser::UnaryOpFactor&>(*factor);

                return codegen_unary_op(unary_op_expression);
            }
            case parser::FactorType::ParenGroup: {
                const auto& paren_group_factor =
                    dynamic_cast<const parser::ParenGroupFactor&>(*factor);

                return codegen_expression(*paren_group_factor.m_expression);
            }
        }
    }

    std::unreachable();
}

[[nodiscard]] std::string codegen_unary_op(
    const parser::UnaryOpFactor& unary_op) {
    switch (unary_op.m_op_type) {
        case parser::UnaryOpType::LogicalNot:
            return fmt::format("{}\ncmp w0, 0\ncset w0, eq",
                               codegen_expression(*unary_op.m_factor));
        case parser::UnaryOpType::BitwiseNot:
            return fmt::format("{}\nmvn w0, w0",
                               codegen_expression(*unary_op.m_factor));
        case parser::UnaryOpType::Negate:
            return fmt::format("{}\nneg w0, w0",
                               codegen_expression(*unary_op.m_factor));
    }
}

}  // namespace codegen
