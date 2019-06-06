#include <assert.h>
#include <fcntl.h>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <cstring>
#include "utils.hpp"
#include <fstream>
#include <cstdarg>

#ifdef WIN32
#include <windows.h>
#include "shine.hpp"
void coloredPrintf(Color col, std::string_view format, ...)
{
    switch (col)
    {
        case Color::Black:break;
        case Color::White:break;
        case Color::Gray:
            SetConsoleTextAttribute(hConsole, 30);
            break;
        case Color::Blue:break;
        case Color::Green:break;
        case Color::Red:break;
    }
    SetConsoleTextAttribute(hConsole, FOREGROUND_BLUE);
    va_list args;
    va_start(args, format);
    vprintf(format.data(), args);
    va_end(args);
    SetConsoleTextAttribute(hConsole, 0);
}
#else

void coloredPrintf(Color color, std::string_view format, ...)
{
    switch (color)
    {
        case Color::Black:
            printf("\e[30m");
            break;
        case Color::White:
            printf("\e[97m");
            break;
        case Color::Gray:
            printf("\e[90m");
            break;
        case Color::Blue:
            break;
        case Color::Green:
            printf("\e[32m");
            break;
        case Color::Red:
            break;
        case Color::Cyan:
            printf("\e[36m");
            break;
    }
    va_list args;
    va_start(args, format);
    vprintf(format.data(), args);
    printf("\e[0m");
}

#endif

/*
 * Return the filesize of `filename` or -1.
 */

size_t file_size(FILE *stream)
{
    fseek(stream, 0, SEEK_END);
    size_t size = ftell(stream);
    rewind(stream);
    return size;
}

/*
 * Read the contents of `filename` or return NULL.
 */

std::vector<char> file_read(const char *filename)
{
    FILE *fh = fopen(filename, "r");

    if (fh == nullptr)
        return std::vector<char>();

    size_t len = file_size(fh);

    std::vector<char> buf(len + 1);

    fread(buf.data(), sizeof(char), len, fh);
    fclose(fh);
    return buf;
}

/*
 * Read `stream` until EOF.
 */

std::vector<char> read_until_eof(FILE *stream)
{
    off_t len = 0;
    char buf[1024];
    std::vector<char> data;
    while (!feof(stream) && !ferror(stream))
    {
        size_t n = fread(buf, 1, 1024, stream);
        len += strlen(buf);
        data.insert(data.end(), buf, buf + n);
    }
    return data;
}
