#pragma once

#include <vector>
#include <string_view>

enum class Color {
    Black,
    White,
    Gray,
    Blue,
    Green,
    Red,
    Cyan,
};

void coloredPrintf(Color, std::string_view, ...);
size_t file_size(FILE *handle);
std::vector<char> file_read(const char *filename);
std::vector<char> read_until_eof(FILE *stream);
