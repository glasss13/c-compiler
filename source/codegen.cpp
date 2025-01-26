#include "codegen.hpp"

#include <algorithm>
#include <iostream>
#include <ranges>

#include "parser.hpp"

namespace {
std::string label(int label_idx) { return fmt::format("L{}", label_idx); }
}  // namespace

namespace codegen {

[[nodiscard]] std::string AArch64Generator::codegen_program(
    const parser::Program& program) {
    std::string out;
    for (const auto& func : program.m_functions) {
        out += codegen_function(*func);
        out += '\n';
    }

    return out;
}

[[nodiscard]] std::string AArch64Generator::codegen_function(
    const parser::Function& function) {
    // function declaration
    if (!function.m_body) {
        m_functions.emplace(
            function.m_name,
            Function{function.m_name, function.m_params.size(), false});
        return "";
    }

    auto it = m_functions.find(function.m_name);
    if (it != m_functions.end()) {
        if (it->second.m_defined) {
            std::cout << fmt::format("function {} already defined",
                                     function.m_name);
            std::terminate();
        }
        if (it->second.arity != function.m_params.size()) {
            std::cout << fmt::format("function {} has mismatched param count",
                                     function.m_name);
            std::terminate();
        }
        it->second.m_defined = true;
    } else {
        m_functions.emplace(
            function.m_name,
            Function{function.m_name, function.m_params.size(), true});
    }

    const bool has_return = std::ranges::any_of(
        function.m_body->m_block_items, [](const auto& item) {
            if (item->m_item_type != parser::BlockItemType::Statement) {
                return false;
            }

            const auto& statement =
                dynamic_cast<const parser::Statement&>(*item);
            return statement.m_statement_type == parser::StatementType::Return;
        });

    auto out = fmt::format(
        ".globl _{}\n"
        "_{}:\n"
        "stp x29, x30, [sp, #-16]!\n"
        "mov x29, sp\n",
        function.m_name, function.m_name);

    std::string param_load_asm;
    auto scope = m_scope->create_child_scope();

    for (size_t i = 0; i < function.m_params.size(); ++i) {
        param_load_asm += fmt::format(
            "sub sp, sp, #16\n"
            "str w{}, [x29, #{}]\n",
            i, scope->base_offset());
        scope = scope->add_var(function.m_params[i]);
    }
    out += param_load_asm;

    enter_scope(scope);
    const std::string body_asm = codegen_block(*function.m_body);
    exit_scope();

    out += body_asm;

    // auto out = fmt::format(
    //     ".globl _{}\n"
    //     "_{}:\n"
    //     "stp x29, x30, [sp, #-16]!\n"
    //     "mov x29, sp\n"
    //     "{}",
    //     function.m_name, function.m_name, body_asm);

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

[[nodiscard]] std::string AArch64Generator::codegen_block(
    const parser::CompoundStatement& statement) {
    std::string asm_out;

    enter_scope(m_scope->create_child_scope());

    for (const auto& block_item : statement.m_block_items) {
        switch (block_item->m_item_type) {
            case parser::BlockItemType::Declaration: {
                const auto& decl =
                    dynamic_cast<const parser::Declaration&>(*block_item);
                asm_out += codegen_declaration(decl);
                asm_out += '\n';
                break;
            }
            case parser::BlockItemType::Statement: {
                const auto& stmnt =
                    dynamic_cast<const parser::Statement&>(*block_item);
                asm_out += codegen_statement(stmnt);
                asm_out += '\n';
                break;
            }
        }
    }

    exit_scope();
    return asm_out;
}

[[nodiscard]] std::string AArch64Generator::codegen_declaration(
    const parser::Declaration& declaration) {
    if (m_scope->local_contains(declaration.m_var_name)) {
        std::cout << fmt::format("Variable {} already declared",
                                 declaration.m_var_name);
        std::terminate();
    }
    const auto base_offset = m_scope->base_offset();
    enter_scope(m_scope->add_var(declaration.m_var_name));

    std::string initializer;
    if (declaration.m_initializer) {
        initializer = codegen_expression(*declaration.m_initializer);
    } else {
        initializer = fmt::format("mov w0, #0");
    }
    return fmt::format(
        "sub sp, sp, #16\n"
        "{}\n"
        "str w0, [x29, #{}]",
        initializer, base_offset);
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
        case parser::StatementType::Expression: {
            const auto& expr_statement =
                dynamic_cast<const parser::ExpressionStatement&>(statement);

            return codegen_expression(*expr_statement.m_expr);
        }
        case parser::StatementType::If: {
            const auto& if_statement =
                dynamic_cast<const parser::IfStatement&>(statement);

            const auto condition_asm =
                codegen_expression(*if_statement.m_condition);
            const auto then_asm = codegen_statement(*if_statement.m_then);
            std::string else_asm;
            if (if_statement.m_else) {
                else_asm = codegen_statement(*if_statement.m_else);
            }

            const auto else_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            return fmt::format(
                "{}\n"
                "cmp w0, #0\n"
                "beq {}\n"
                "{}\n"
                "b {}\n"
                "{}:\n"
                "{}\n"
                "{}:",
                condition_asm, else_label, then_asm, end_label, else_label,
                else_asm, end_label);
        }
        case parser::StatementType::Compound: {
            const auto& compound_statement =
                dynamic_cast<const parser::CompoundStatement&>(statement);
            return codegen_block(compound_statement);
        }
        case parser::StatementType::For: {
            const auto& for_statement =
                dynamic_cast<const parser::ForStatement&>(statement);

            const auto loop_label = label(get_label_idx());
            const auto updater_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            enter_scope(m_scope->add_continue_label(updater_label)
                            ->add_break_label(end_label));

            const auto init_asm = codegen_expression(*for_statement.m_initial);
            const auto cond_asm = codegen_expression(*for_statement.m_control);
            const auto updater_asm = codegen_expression(*for_statement.m_post);

            const auto body_asm = codegen_statement(*for_statement.m_statement);

            return fmt::format(
                "{}\n"   // init_asm
                "{}:\n"  // loop_label
                "{}\n"   // cond_asm
                "cmp w0, #0\n"
                "beq {}\n"  // end_label
                "{}\n"      // body_asm
                "{}:\n"     // updater_label
                "{}\n"      // updater_asm
                "b {}\n"    // loop_label
                "{}:\n",    // loop_end
                init_asm, loop_label, cond_asm, end_label, body_asm,
                updater_label, updater_asm, loop_label, end_label);
        }
        case parser::StatementType::ForDecl: {
            const auto& for_decl =
                dynamic_cast<const parser::ForDeclStatement&>(statement);

            const auto loop_label = label(get_label_idx());
            const auto updater_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            enter_scope(m_scope->create_child_scope()
                            ->add_continue_label(updater_label)
                            ->add_break_label(end_label));

            const auto init_asm = codegen_declaration(*for_decl.m_initial);
            const auto cond_asm = codegen_expression(*for_decl.m_control);
            const auto updater_asm = codegen_expression(*for_decl.m_post);

            const auto body_asm = codegen_statement(*for_decl.m_statement);

            exit_scope();

            return fmt::format(
                "{}\n"   // init_asm
                "{}:\n"  // loop_label
                "{}\n"   // cond_asm
                "cmp w0, #0\n"
                "beq {}\n"  // end_label
                "{}\n"      // body_asm
                "{}:\n"     // updater_label
                "{}\n"      // updater_asm
                "b {}\n"    // loop_label
                "{}:\n",    // loop_end
                init_asm, loop_label, cond_asm, end_label, body_asm,
                updater_label, updater_asm, loop_label, end_label);
        }
        case parser::StatementType::While: {
            const auto& while_statement =
                dynamic_cast<const parser::WhileStatement&>(statement);
            const auto loop_label = label(get_label_idx());
            const auto updater_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            // TODO this abstraction needs to be changed
            enter_scope(m_scope->add_continue_label(loop_label)
                            ->add_break_label(end_label));

            const auto body_asm =
                codegen_statement(*while_statement.m_statement);
            const auto cond_asm =
                codegen_expression(*while_statement.m_cond_expr);

            return fmt::format(
                "{}:\n"  // loop label
                "{}\n"   // cond_asm
                "cmp w0, #0\n"
                "beq {}\n"  // end loop
                "{}\n"      // body_asm
                "b {}\n"    // loop label
                "{}:",      // end loop
                loop_label, cond_asm, end_label, body_asm, loop_label,
                end_label);
        }
        case parser::StatementType::Do: {
            const auto& do_statement =
                dynamic_cast<const parser::DoStatement&>(statement);
            const auto loop_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            enter_scope(m_scope->add_continue_label(loop_label)
                            ->add_break_label(end_label));

            const auto body_asm = codegen_statement(*do_statement.m_statement);
            const auto cond_asm = codegen_expression(*do_statement.m_cond_expr);

            return fmt::format(
                "{}:\n"  // loop label
                "{}\n"   // body asm
                "{}\n"   // cond_asm
                "cmp w0, #0\n"
                "bne {}\n"  // loop label
                "{}:",
                loop_label, body_asm, cond_asm, loop_label, end_label);
        }
        case parser::StatementType::Break: {
            const auto break_label = m_scope->get_break_label();
            if (!break_label) {
                std::cout << "Must use break in a loop context\n";
                std::terminate();
            }

            return fmt::format("b {}", *break_label);
        }
        case parser::StatementType::Continue: {
            std::cout << "generating contunue\n";
            const auto continue_label = m_scope->get_continue_label();
            if (!continue_label) {
                std::cout << "Must use continue in a loop context\n";
                std::terminate();
            }

            return fmt::format("b {}", *continue_label);
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
        case parser::ExpressionType::Ternary: {
            const auto& ternary =
                dynamic_cast<const parser::TernaryExpression&>(expr);

            const auto condition_asm = codegen_expression(*ternary.m_cond);
            const auto then_asm = codegen_expression(*ternary.m_then);
            const auto else_asm = codegen_expression(*ternary.m_else);

            const auto else_label = label(get_label_idx());
            const auto end_label = label(get_label_idx());

            return fmt::format(
                "{}\n"
                "cmp w0, #0\n"
                "beq {}\n"
                "{}\n"
                "b {}\n"
                "{}:\n"
                "{}\n"
                "{}:",
                condition_asm, else_label, then_asm, end_label, else_label,
                else_asm, end_label);
        }
        case parser::ExpressionType::FunctionCall: {
            const auto& call_expr =
                dynamic_cast<const parser::FunctionCallExpression&>(expr);

            auto it = m_functions.find(call_expr.m_func_name);
            if (it == m_functions.end()) {
                std::cout << fmt::format("function {} not declared",
                                         call_expr.m_func_name);
                std::terminate();
            }
            if (it->second.arity != call_expr.m_arguments.size()) {
                std::cout << fmt::format(
                    "function {} expected {} arguments, got {}",
                    call_expr.m_func_name, it->second.arity,
                    call_expr.m_arguments.size());
                std::terminate();
            }

            // TODO handle moving arguments to stack
            if (call_expr.m_arguments.size() > 8) {
                std::cout << "More than 8 arguments not supported rn\n";
                std::terminate();
            }

            std::string out_asm;

            std::cout << "call expr size: " << call_expr.m_arguments.size();

            for (const auto& arg_expr : call_expr.m_arguments) {
                std::cout << "genning arg expr\n";
                const auto arg_asm = codegen_expression(*arg_expr);
                out_asm += fmt::format(
                    "{}\n"  // arg_asm
                    "str w0, [sp, #-16]!\n",
                    arg_asm);
            }

            for (int i = static_cast<int>(call_expr.m_arguments.size()) - 1;
                 i >= 0; --i) {
                std::cout << "pushing to stack\n";
                out_asm += fmt::format("ldr w{}, [sp], #16\n", i);
            }

            out_asm += fmt::format("bl _{}", call_expr.m_func_name);

            return out_asm;
        }

        case parser::ExpressionType::Empty: {
            return "";
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
        case parser::UnaryOpType::PreIncrement: {
            if (unary_op.m_expr->m_expr_type !=
                parser::ExpressionType::VariableRef) {
                std::cout << "Pre increment must be on a variable\n";
                std::terminate();
            }
            const auto& ref =
                dynamic_cast<const parser::VariableRefExpression&>(
                    *unary_op.m_expr);
            const auto base_offset = m_scope->lookup(ref.m_var_name);
            if (!base_offset) {
                std::cout << "Unknown variable: " << ref.m_var_name;
                std::terminate();
            }
            return fmt::format(
                "ldr w0, [x29, #{}]\n"
                "add w0, w0, #1\n"
                "str w0, [x29, #{}]",
                *base_offset, *base_offset);
        }
        case parser::UnaryOpType::PreDecrement: {
            if (unary_op.m_expr->m_expr_type !=
                parser::ExpressionType::VariableRef) {
                std::cout << "Pre decrement must be on a variable\n";
                std::terminate();
            }
            const auto& ref =
                dynamic_cast<const parser::VariableRefExpression&>(
                    *unary_op.m_expr);
            const auto base_offset = m_scope->lookup(ref.m_var_name);
            if (!base_offset) {
                std::cout << "Unknown variable: " << ref.m_var_name;
                std::terminate();
            }
            return fmt::format(
                "ldr w0, [x29, #{}]\n"
                "sub w0, w0, #1\n"
                "str w0, [x29, #{}]",
                *base_offset, *base_offset);
        }
        case parser::UnaryOpType::PostIncrement: {
            if (unary_op.m_expr->m_expr_type !=
                parser::ExpressionType::VariableRef) {
                std::cout << "Post increment must be on a variable\n";
                std::terminate();
            }
            const auto& ref =
                dynamic_cast<const parser::VariableRefExpression&>(
                    *unary_op.m_expr);
            const auto base_offset = m_scope->lookup(ref.m_var_name);
            if (!base_offset) {
                std::cout << "Unknown variable: " << ref.m_var_name;
                std::terminate();
            }
            return fmt::format(
                "ldr w0, [x29, #{}]\n"
                "mov w1, w0\n"
                "add w0, w0, #1\n"
                "str w0, [x29, #{}]\n"
                "mov w0, w1",
                *base_offset, *base_offset);
        }
        case parser::UnaryOpType::PostDecrement: {
            if (unary_op.m_expr->m_expr_type !=
                parser::ExpressionType::VariableRef) {
                std::cout << "Post decrement must be on a variable\n";
                std::terminate();
            }
            const auto& ref =
                dynamic_cast<const parser::VariableRefExpression&>(
                    *unary_op.m_expr);
            const auto base_offset = m_scope->lookup(ref.m_var_name);
            if (!base_offset) {
                std::cout << "Unknown variable: " << ref.m_var_name;
                std::terminate();
            }
            return fmt::format(
                "ldr w0, [x29, #{}]\n"
                "mov w1, w0\n"
                "sub w0, w0, #1\n"
                "str w0, [x29, #{}]\n"
                "mov w0, w1",
                *base_offset, *base_offset);
        }
    }
}

}  // namespace codegen
