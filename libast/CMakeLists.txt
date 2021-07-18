add_library(shineast parser.cpp exception.cpp)
target_link_libraries(shineast shinelexer shineutils)
