#include "codegen.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

namespace boost {
[[noreturn]] void throw_exception(std::exception const& e) {
    std::cerr << "Boost exception: " << e.what() << std::endl;
    std::terminate();
}
}  // namespace boost

#define BOOST_NO_EXCEPTIONS
#include <boost/process.hpp>
#include <catch2/catch_test_macros.hpp>

const std::filesystem::path valid_programs_path =
    "../../../test/programs/valid";

namespace bp = boost::process;

// NOLINTBEGIN
TEST_CASE("Compile and check return codes",
          "[lex][parse][codegen][integration]") {
    auto tmp_dir = std::filesystem::temp_directory_path();

    for (const auto& entry :
         std::filesystem::directory_iterator(valid_programs_path)) {
        if (entry.is_regular_file() && entry.path().extension() == ".c") {
            std::ifstream file(entry.path());
            auto reference_output =
                tmp_dir / ("reference-" + entry.path().filename().string());
            auto assembly_out_path =
                tmp_dir / ("asm-" + entry.path().filename().string() + ".s");
            auto our_output =
                tmp_dir / ("our-" + entry.path().filename().string());

            auto lexed = lexer::lex_stream(file);
            auto it = lexed.begin();
            const auto parsed_ast = parser::parse_program(it);
            REQUIRE(parsed_ast);
            auto codegen = codegen::codegen_program(**parsed_ast);

            std::ofstream asm_stream(assembly_out_path);
            asm_stream << codegen << std::endl;

            bp::system(fmt::format("g++ {} -o {}", assembly_out_path.string(),
                                   our_output.string()));
            bp::system(fmt::format("g++ {} -o {}", entry.path().string(),
                                   reference_output.string()));

            auto our_ret_code = bp::system(our_output.string());
            auto reference_ret_code = bp::system(reference_output.string());

            REQUIRE(reference_ret_code == our_ret_code);
        }
    }
}

// NOLINTEND
