cmake_minimum_required(VERSION 3.14)

include(cmake/prelude.cmake)

project(
    c-compiler
    VERSION 0.1.0
    DESCRIPTION "C compiler"
    HOMEPAGE_URL "https://example.com/"
    LANGUAGES CXX
)

include(cmake/project-is-top-level.cmake)
include(cmake/variables.cmake)

# ---- Declare library ----

add_library(
    c-compiler_lib OBJECT
    source/lib.cpp
    source/lexer.cpp
)

target_include_directories(
    c-compiler_lib ${warning_guard}
    PUBLIC
    "\$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/source>"
)

target_compile_features(c-compiler_lib PUBLIC cxx_std_23)

find_package(fmt REQUIRED)
target_link_libraries(c-compiler_lib PRIVATE fmt::fmt)

# ---- Declare executable ----

add_executable(c-compiler_exe source/main.cpp)
add_executable(c-compiler::exe ALIAS c-compiler_exe)

set_property(TARGET c-compiler_exe PROPERTY OUTPUT_NAME c-compiler)

target_compile_features(c-compiler_exe PRIVATE cxx_std_23)

target_link_libraries(c-compiler_exe PRIVATE c-compiler_lib)

# ---- Install rules ----

if(NOT CMAKE_SKIP_INSTALL_RULES)
  include(cmake/install-rules.cmake)
endif()

# ---- Developer mode ----

if(NOT c-compiler_DEVELOPER_MODE)
  return()
elseif(NOT PROJECT_IS_TOP_LEVEL)
  message(
      AUTHOR_WARNING
      "Developer mode is intended for developers of c-compiler"
  )
endif()

include(cmake/dev-mode.cmake)
