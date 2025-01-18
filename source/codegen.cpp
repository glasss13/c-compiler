#include "codegen.hpp"

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
            break;
    }
}
[[nodiscard]] std::string codegen_expression(
    const parser::Expression& expression) {
    switch (expression.m_expression_type) {
        case parser::ExpressionType::Constant: {
            const auto& const_expression =
                dynamic_cast<const parser::ConstantExpression&>(expression);

            return fmt::format("mov w0, #{}", const_expression.m_constant);
        }
        case parser::ExpressionType::UnaryOp: {
            const auto& unary_op_expression =
                dynamic_cast<const parser::UnaryOpExpression&>(expression);

            return codegen_unary_op(unary_op_expression);
        }
    }
}

[[nodiscard]] std::string codegen_unary_op(
    const parser::UnaryOpExpression& unary_op) {
    switch (unary_op.m_op_type) {
        case parser::UnaryOpType::LogicalNot:
            return fmt::format("{}\ncmp w0, 0\ncset w0, eq",
                               codegen_expression(*unary_op.m_expression));
        case parser::UnaryOpType::BitwiseNot:
            return fmt::format("{}\nmvn w0, w0",
                               codegen_expression(*unary_op.m_expression));
        case parser::UnaryOpType::Negate:
            return fmt::format("{}\nneg w0, w0",
                               codegen_expression(*unary_op.m_expression));
    }
}

}  // namespace codegen
