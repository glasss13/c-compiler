# Parent project does not export its library target, so this CML implicitly
# depends on being added from it, i.e. the testing is done only from the build
# tree and is not feasible from an install location

project(c-compilerTests LANGUAGES CXX)

# ---- Dependencies ----

find_package(Catch2 REQUIRED)
include(Catch)

# ---- Tests ----

add_executable(c-compiler_test source/integration/lex_parse.cpp source/integration/codegen.cpp source/unit/lex.cpp)
target_link_libraries(
    c-compiler_test PRIVATE
    c-compiler_lib
    Catch2::Catch2WithMain
)
target_compile_features(c-compiler_test PRIVATE cxx_std_23)

catch_discover_tests(c-compiler_test)

# ---- End-of-file commands ----

add_folders(Test)
