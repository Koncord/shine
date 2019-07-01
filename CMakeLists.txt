cmake_minimum_required(VERSION 3.13)
project(shine)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/)
include(PVS-Studio)

find_package(LLVM 8.0 REQUIRED)

include_directories(
        SYSTEM ${LLVM_INCLUDE_DIRS}
        src
)

#add_definitions(-DDEBUG_PARSER)

FILE(GLOB SRCSPP
        src/*.cpp
        src/*/*.cpp
        src/*/*/*.cpp
        )

add_executable(shine ${SRCS} ${SRCSPP})

target_link_libraries(shine ${LLVM_LIBRARIES} ${LLVM_LDFLAGS})

option(ENABLE_PVS_OPTION "Enable PVS Studio" FALSE)

if (ENABLE_PVS_OPTION)
    pvs_studio_add_target(TARGET shine.analyze ALL
            OUTPUT FORMAT errorfile
            ANALYZE shine
            LOG shine.err)
endif (ENABLE_PVS_OPTION)
