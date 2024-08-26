# This software is distributed under the terms of the MIT License.
# Copyright (c) 2024 Zubax Robotics  <info@zubax.com>

cmake_minimum_required(VERSION 3.12)
project(olg_scheduler_tests C CXX)
enable_testing()

set(CTEST_OUTPUT_ON_FAILURE ON)

# Use -DNO_STATIC_ANALYSIS=1 to suppress static analysis. If not suppressed, the tools used here shall be available.
if (NOT NO_STATIC_ANALYSIS)
    find_program(clang_tidy NAMES clang-tidy)
    if (NOT clang_tidy)
        message(FATAL_ERROR "Could not locate clang-tidy")
    endif ()
    message(STATUS "Using clang-tidy: ${clang_tidy}")
    set(CMAKE_CXX_CLANG_TIDY ${clang_tidy})
endif ()

find_program(clang_format NAMES clang-format)
if (NOT clang_format)
    message(STATUS "Could not locate clang-format")
else ()
    file(GLOB format_files
            ${CMAKE_CURRENT_SOURCE_DIR}/include/*.[ch]pp
    )
    message(STATUS "Using clang-format: ${clang_format}; files: ${format_files}")
    add_custom_target(format COMMAND ${clang_format} -i -fallback-style=none -style=file --verbose ${format_files})
endif ()

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.15.2
)
FetchContent_MakeAvailable(googletest)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -pedantic -fstrict-aliasing")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wdouble-promotion -Wswitch-enum -Wfloat-equal -Wundef")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wconversion -Wsign-promo")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wsign-conversion -Wcast-align -Wmissing-declarations")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wtype-limits -Wnon-virtual-dtor -Woverloaded-virtual")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-attributes")

add_executable(test_olg_scheduler_cpp20 ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_olg_scheduler.cpp)
set_target_properties(test_olg_scheduler_cpp20 PROPERTIES CXX_STANDARD 20)
target_include_directories(test_olg_scheduler_cpp20 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include/olg_scheduler)
target_include_directories(test_olg_scheduler_cpp20 SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/lib/cavl)
target_link_libraries(test_olg_scheduler_cpp20
        PRIVATE
        GTest::gmock_main
)

include(GoogleTest)
gtest_discover_tests(test_olg_scheduler_cpp20)
