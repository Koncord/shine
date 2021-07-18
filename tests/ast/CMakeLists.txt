add_library(tests_main test.cpp)
target_link_libraries(tests_main PRIVATE Catch2::Catch2)

add_executable(ast_tests test_lexer.cpp test_ast.cpp test_loops.cpp test_branches.cpp test_comments.cpp test_arrays.cpp)
target_link_libraries(ast_tests PRIVATE Catch2::Catch2 tests_main shineast shineutils)
target_precompile_headers(ast_tests PRIVATE pch.h)

catch_discover_tests(ast_tests)
