#pragma once

#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>

#include "codegen.hpp"
#include "lexer.hpp"
#include "lib.hpp"
#include "parser.hpp"

namespace boost {
[[noreturn]] inline void throw_exception(std::exception const& e) {
    std::cerr << "Boost exception: " << e.what() << std::endl;
    std::terminate();
}
}  // namespace boost

#define BOOST_NO_EXCEPTIONS
#include <boost/process.hpp>

namespace bp = boost::process;
namespace fs = boost::filesystem;

inline int compile_and_run(const std::string& program, bool is_asm) {
    auto temp_dir = fs::temp_directory_path();
    auto inp_path = temp_dir / fs::unique_path("input-%%%%-%%%%-%%%%");
    auto out_path = temp_dir / fs::unique_path("output-%%%%-%%%%-%%%%");
    if (is_asm) {
        inp_path += ".s";
    } else {
        inp_path += ".c";
    }

    std::ofstream inp_file(inp_path.string());
    inp_file << program << std::endl;

    const auto cmd =
        fmt::format("g++ {} -o {}", inp_path.string(), out_path.string());
    bp::system(cmd.c_str());

    return bp::system(out_path.c_str());
}
