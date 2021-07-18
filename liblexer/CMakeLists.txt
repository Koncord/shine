add_library(shinelexer lexer.cpp token.cpp exception.cpp)
target_link_libraries(shinelexer shineutils)
