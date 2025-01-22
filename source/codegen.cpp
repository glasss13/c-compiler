#include "codegen.hpp"

#include <iostream>

#include "parser.hpp"

namespace {
std::string label(int label_idx) { return fmt::format("L{}", label_idx); }
}  // namespace

namespace codegen {

[[nodiscard]] std::string AArch64Generator::codegen_program(
    const parser::Program& program) {
    return codegen_function(*program.m_function);
}

[[nodiscard]] std::string AArch64Generator::codegen_function(
    const parser::Function& function) {
    enter_scope(m_scope->create_child_scope());

    std::string statements;
    bool has_return = false;
    for (const auto& statement : function.m_statements) {
        if (statement->m_statement_type == parser::StatementType::Return) {
            has_return = true;
        }
        statements += codegen_statement(*statement);
        statements += '\n';
    }

    auto out = fmt::format(
        ".globl _{}\n"
        "_{}:\n"
        "stp x29, x30, [sp, #-16]!\n"
        "mov x29, sp\n"
        "{}",
        function.m_name, function.m_name, statements);

    // C standard says that main function should return 0 even if no
    // return statement is present.
    // If the function isn't the main function, using the return value is
    // UB. So lets just return 0 either way.
    if (!has_return) {
        out +=
            "\nmov  w0, #0\n"
            "mov sp, x29\n"
            "ldp x29, x30, [sp], #16\n"
            "ret";
    }

    return out;
}
[[nodiscard]] std::string AArch64Generator::codegen_statement(
    const parser::Statement& statement) {
    switch (statement.m_statement_type) {
        case parser::StatementType::Return: {
            const auto& ret_statement =
                dynamic_cast<const parser::ReturnStatement&>(statement);

            return fmt::format(
                "{}\n"
                "mov sp, x29\n"
                "ldp x29, x30, [sp], #16\n"
                "ret",
                codegen_expression(*ret_statement.m_expr));
        }
        case parser::StatementType::Declaration: {
            const auto& dec_statement =
                dynamic_cast<const parser::DeclarationStatement&>(statement);

            // TODO: clean this up
            const auto base_offset = m_scope->base_offset();
            enter_scope(m_scope->add_var(dec_statement.m_var_name));

            auto declaration = fmt::format("sub sp, sp, #16");

            std::string initializer;
            if (dec_statement.m_initializer) {
                initializer = codegen_expression(*dec_statement.m_initializer);
            } else {
                initializer = fmt::format("mov w0, #0");
            }
            return fmt::format(
                "{}\n"
                "{}\n"
                "str w0, [x29, #{}]",
                declaration, initializer, base_offset);
        }
        case parser::StatementType::Expression: {
            const auto& expr_statement =
                dynamic_cast<const parser::ExpressionStatement&>(statement);

            return codegen_expression(*expr_statement.m_expr);
        }
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_expression(
    const parser::Expression& expr) {
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
        case parser::ExpressionType::Assignment: {
            const auto& assignment_expr =
                dynamic_cast<const parser::AssignmentExpression&>(expr);

            const auto base_offset =
                m_scope->lookup(assignment_expr.m_var_name);
            if (!base_offset) {
                std::cerr << "Unknown variable: " << assignment_expr.m_var_name
                          << '\n';
                std::terminate();
            }

            return fmt::format(
                "{}\n"
                "str w0, [x29, #{}]",
                codegen_expression(*assignment_expr.m_expr), *base_offset);
        }

        case parser::ExpressionType::CompoundAssignment: {
            const auto& compound_expr =
                dynamic_cast<const parser::CompoundAssignmentExpression&>(expr);
            return codegen_compound_op(compound_expr);
        }
        case parser::ExpressionType::VariableRef: {
            const auto& ref_expression =
                dynamic_cast<const parser::VariableRefExpression&>(expr);

            const auto base_offset = m_scope->lookup(ref_expression.m_var_name);
            if (!base_offset) {
                std::cerr << "Unknown variable: " << ref_expression.m_var_name
                          << '\n';
                std::terminate();
            }

            return fmt::format("ldr w0, [x29, #{}]", *base_offset);
        }
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_binary_op(
    const parser::BinaryOpExpression& expr) {
    const auto lhs_gen = codegen_expression(*expr.m_lhs);
    const auto rhs_gen = codegen_expression(*expr.m_rhs);
    switch (expr.m_op_type) {
        case parser::BinaryOpType::Add: {
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nadd w0, "
                "w0, w1",
                lhs_gen, rhs_gen);
        }
        case parser::BinaryOpType::Multiply: {
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nmul w0, "
                "w0, w1",
                lhs_gen, rhs_gen);
        }
        case parser::BinaryOpType::Subtract:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nsub w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::Divide:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nsdiv w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::Modulo:
            return fmt::format(
                "{}\n"
                "str w0, [sp, #-16]!\n"
                "{}\n"
                "ldr w1, [sp], #16\n"
                "sdiv w2, w1, w0\n"
                "msub w0, w2, w0, w1\n",
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
        case parser::BinaryOpType::BitwiseAnd:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nand w0, "
                "w1, w0",
                lhs_gen, rhs_gen);

        case parser::BinaryOpType::BitwiseOr:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\norr w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::BitwiseXor:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\neor w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::RightShift:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nasr w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::LeftShift:
            return fmt::format(
                "{}\nstr w0, [sp, "
                "#-16]!\n{}\nldr w1, "
                "[sp], #16\nlsl w0, "
                "w1, w0",
                lhs_gen, rhs_gen);
        case parser::BinaryOpType::Comma:
            return fmt::format(
                "{}\n"
                "{}",
                lhs_gen, rhs_gen);
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_compound_op(
    const parser::CompoundAssignmentExpression& expr) {
    const auto base_offset = m_scope->lookup(expr.m_var_name);
    if (!base_offset) {
        std::cerr << "Unknown variable: " << expr.m_var_name << '\n';
        std::terminate();
    }
    const auto rhs_gen = codegen_expression(*expr.m_expr);

    switch (expr.m_op_type) {
        case parser::CompoundAssignmentType::PlusEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "add w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::MinusEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "sub w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::TimesEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "mul w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::DivideEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "sdiv w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::ModuloEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "sdiv w2, w1, w0\n"
                "msub w0, w2, w0, w1\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::LeftShiftEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "lsl w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::RightShiftEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "asr w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::BitwiseAndEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "and w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::BitwiseOrEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "orr w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
        case parser::CompoundAssignmentType::XorEqual:
            return fmt::format(
                "{}\n"
                "ldr w1, [x29, #{}]\n"
                "eor w0, w1, w0\n"
                "str w0, [x29, #{}]",
                rhs_gen, *base_offset, *base_offset);
    }
}

[[nodiscard]] std::string AArch64Generator::codegen_unary_op(
    const parser::UnaryOpExpression& unary_op) {
    switch (unary_op.m_op_type) {
        case parser::UnaryOpType::LogicalNot:
            return fmt::format(
                "{}\ncmp w0, 0\ncset w0, "
                "eq",
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
