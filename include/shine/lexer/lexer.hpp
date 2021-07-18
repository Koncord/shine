#pragma once

#include "token.hpp"
#include <vector>
#include <string>
#include <utils/position.hpp>

// Lexer struct.
namespace shine {
    class Lexer {
        friend class LexerException;
        friend class ParseException;
        //friend class Parser;

    private:
        TokenType scan_ident(int c);

        int hex_literal();

        char scan_char(int c);
        TokenType scan_string();

        TokenType scan_number(int c);

        int undo();
        int next();
        bool tryNext(int &result);
        bool tryUndo(int &result);
        void newLine();
        TokenType scan();
    public:
        Lexer(std::vector<char> source, const char *filename) : source(std::move(source)), filename(filename) {}

        Lexer(std::string const &source, const char *filename) : source(source.begin(), source.end()),
                                                                 filename(filename) {}

        int getLine() const { return lineno; }

        int getLinePosition() const { return linepos; }

        Position getPosition() const { return Position{getLine(), getLinePosition()}; }

        Token getToken() const { return tok; }

        bool isType(TokenType t) { return tok.type == t; }


        bool isEquality() const {
            return shine::isEquality(tok.type);
        }

        bool isRelational() const {
            return shine::isRelational(tok.type);
        }

        const Token &getNextToken() {
            scan();
            return tok;
        }

        void inspect() const;

    private:
        std::vector<char> source;
        const char *filename;
        int stash = 0;
        int lineno = 1;
        int linepos = 1;
        size_t offset = 0;
        Token tok{};
    };
}