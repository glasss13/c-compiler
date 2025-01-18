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
    switch (statement.m_type) {
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
    switch (expression.m_type) {
        case parser::ExpressionType::Constant:
            const auto& const_expression =
                dynamic_cast<const parser::ConstantExpression&>(expression);

            return fmt::format("mov w0, #{}", const_expression.m_constant);
    }
}

}  // namespace codegen
