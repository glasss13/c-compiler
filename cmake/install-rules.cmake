install(
    TARGETS c-compiler_exe
    RUNTIME COMPONENT c-compiler_Runtime
)

if(PROJECT_IS_TOP_LEVEL)
  include(CPack)
endif()
