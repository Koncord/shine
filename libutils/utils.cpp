#include <assert.h>
#include <fcntl.h>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <cstring>
#include <utils/utils.hpp>
#include <fstream>
#include <cstdarg>

#ifdef WIN32

#include <windows.h>

static std::string GetLastErrorAsString() {
    DWORD errorMessageID = GetLastError();
    if (errorMessageID == 0)
        return std::string();

    LPSTR messageBuffer = nullptr;

    size_t size = FormatMessageA(
            FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
            nullptr,
            errorMessageID,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPSTR) &messageBuffer,
            0,
            nullptr);

    std::string message(messageBuffer, size);
    LocalFree(messageBuffer);

    return message;
}

#endif

void coloredPrintf(Color color, std::string_view format, ...) {
#ifdef WIN32
    static bool initialized = false;
    // initialize VT100 mode
    if (!initialized) {
        initialized = true;
        auto hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD dwMode = 0;
        if (!GetConsoleMode(hConsole, &dwMode))
            throw std::runtime_error(GetLastErrorAsString());
        if (!SetConsoleMode(hConsole, dwMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING))
            throw std::runtime_error(GetLastErrorAsString());
    }
#endif
    switch (color) {
        case Color::Black:
            printf("\x1b[30m");
            break;
        case Color::White:
            printf("\x1b[97m");
            break;
        case Color::Gray:
            printf("\x1b[90m");
            break;
        case Color::Blue:
            printf("\x1b[34m");
            break;
        case Color::Green:
            printf("\x1b[32m");
            break;
        case Color::Red:
            printf("\x1b[31m");
            break;
        case Color::Cyan:
            printf("\x1b[36m");
            break;
    }
    va_list args;
    va_start(args, format);
    vprintf(format.data(), args);
    printf("\x1b[0m");
}

/*
 * Return the filesize of `filename` or -1.
 */

size_t file_size(FILE *stream) {
    fseek(stream, 0, SEEK_END);
    size_t size = ftell(stream);
    rewind(stream);
    return size;
}

/*
 * Read the contents of `filename` or return NULL.
 */

std::vector<char> file_read(const char *filename) {
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

std::vector<char> read_until_eof(FILE *stream) {
    size_t len = 0;
    char buf[1024];
    std::vector<char> data;
    while (!feof(stream) && !ferror(stream)) {
        size_t n = fread(buf, 1, 1024, stream);
        len += strlen(buf);
        data.insert(data.end(), buf, buf + n);
    }
    return data;
}
