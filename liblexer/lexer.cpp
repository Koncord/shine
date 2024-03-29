#include <cmath>
#include <string>
#include <vector>

#include <lexer/lexer.hpp>
#include <utils/utils.hpp>
#include <utils/exception.hpp>

// Assign token `t`.
#define token(t) (tok.type = TokenType::t)

// True if the lexer should insert a semicolon after `t`.
constexpr bool need_semi(shine::TokenType t) {
    return t == shine::TokenType::Id || t == shine::TokenType::Float
           || t == shine::TokenType::Number || t == shine::TokenType::Char
           || t == shine::TokenType::Boolean
           || t == shine::TokenType::String || t == shine::TokenType::Return;
}

// Convert hex digit `c` to a base 10 int, returning -1 on failure.
static int hex(const char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

namespace shine {
// Scan identifier.
    TokenType Lexer::scan_ident(int c) {
        std::string buf;
        token(Id);
        do {
            buf.push_back((char) c);
        } while (tryNext(c) && (isalpha(c) || isdigit(c) || '_' == c));
        undo();
        switch (buf.size()) {
            case 2:
                if (buf == "if") return token(If);
                if (buf == "as") return token(As);
                break;
            case 3:
                if (buf == "use") return token(Use);
                if (buf == "for") return token(For);
                if (buf == "def") return token(Def);
                if (buf == "end") return token(End);
                if (buf == "let") return token(Let);
                if (buf == "not") return token(OpLNot);
                if (buf == "mod") return token(Mod);
                if (buf == "pub") return token(Pub);
                break;
            case 4:
                if (buf == "case") return token(Case);
                if (buf == "when") return token(When);
                if (buf == "else") return token(Else);
                if (buf == "true") // todo: move out here
                {
                    tok.value = true;
                    return token(Boolean);
                }
                break;
            case 5:
                if (buf == "break") return token(Break);
                if (buf == "const") return token(Const);
                if (buf == "until") return token(Until);
                if (buf == "while") return token(While);
                if (buf == "false")  // todo: move out here
                {
                    tok.value = false;
                    return token(Boolean);
                }
                break;
            case 6:
                if (buf == "sizeof") return token(SizeOf);
                if (buf == "struct") return token(Struct);
                if (buf == "return") return token(Return);
                if (buf == "repeat") return token(Repeat);
                if (buf == "unless") return token(Unless);
                if (buf == "extern") return token(Extern);
                break;
            default:
                if (buf == "continue") return token(Continue);
        }

        tok.value = buf; // id
        return tok.type;
    }

// Scan string hex literal, returning -1 on invalid digits.
    int Lexer::hex_literal() {
        int a = hex(next());
        int b = hex(next());
        if (a > -1 && b > -1) return a << 4 | b;
        throw LexerException(this, "string hex literal \\x contains invalid digits");
    }

    char Lexer::scan_char(int c) {
        switch (c) {
            case '\n':
                newLine();
                break;
            case '\\':
                switch (c = next()) {
                    case '0':
                        c = '\0';
                        break;
                    case 'a':
                        c = '\a';
                        break;
                    case 'b':
                        c = '\b';
                        break;
                    case 'e': // GNU extension
                        c = '\033';
                        break;
                    case 'f':
                        c = '\f';
                        break;
                    case 'n':
                        c = '\n';
                        break;
                    case 'r':
                        c = '\r';
                        break;
                    case 't':
                        c = '\t';
                        break;
                    case 'v':
                        c = '\v';
                        break;
                    case 'x':
                        c = hex_literal();
                        break;
                    default:
                        return c;
                }
                break;
            default:
                return c;
        }
        return c;
    }

// Scan string.
    TokenType Lexer::scan_string() {
        int c;
        std::string buf;
        token(String);

        while (tryNext(c) && ('\"' != (c))) {
            c = scan_char(c);
            buf.push_back(c);
        }

        tok.value = buf;
        return tok.type;
    }

// Scan number.
    TokenType Lexer::scan_number(int c) {
        int64_t n = 0, type = 0, expo = 0;
        double e;
        int expo_type = 1;
        /* expo_type:
         * 1 -> '+'(default)
         * 0 -> '-'
         */
        token(Number);

        if (c == '0')
            goto scan_hex;
        else
            goto scan_int;

        scan_hex:
        if ((c = next()) == 'x') {
            if (!isxdigit(c = next())) {
                throw LexerException(this, "hex literal expects one or more digits");
            } else {
                do n = n << 4 | hex(c);
                while (tryNext(c) && isxdigit(c));
            }
            tok.value = n;
            undo();
            return tok.type;
        } else {
            undo();
            c = '0';
            goto scan_int;
        }

        // [0-9_]+

        scan_int:
        do {
            if ('_' == c) continue;
            else if ('.' == c) goto scan_float;
            else if ('e' == c || 'E' == c) goto scan_expo;
            n = n * 10 + c - '0';
        } while (tryNext(c) && (isdigit(c) || '_' == c || '.' == c || 'e' == c || 'E' == c));
        undo();
        tok.value = n;
        return tok.type;

        // [0-9_]+

        scan_float:
        {
            e = 1.0;
            type = 1;
            token(Float);
            while (tryNext(c) && (isdigit(c) || '_' == c || 'e' == c || 'E' == c)) {
                if ('_' == c) continue;
                else if ('e' == c || 'E' == c) goto scan_expo;
                n = n * 10 + c - '0';
                e *= 10.0;
            }
            if ('f' == c)
                tok.value = (float) n / e;
            else {
                undo();
                tok.value = (double) n / e;
            }
            return tok.type;
        }

        // [\+\-]?[0-9]+

        scan_expo:
        {
            while (tryNext(c) && (isdigit(c) || '+' == c || '-' == c)) {
                if ('-' == c) {
                    expo_type = 0;
                    continue;
                }
                expo = expo * 10 + c - '0';
            }

            undo();
            if (expo_type == 0) expo *= -1;
            if (type == 0)
                tok.value = n * pow(10, expo);
            else
                tok.value = ((float) n / e) * pow(10, expo);
        }

        return tok.type;
    }

// Scan the _next token in the stream, returns 0 on EOS, Illegal token, or a syntax error.

    TokenType Lexer::scan() {
        int c;

        // scan
        scan:
        if (!tryNext(c))
            return token(EOS);
        if (offset >= source.size())
            return token(EOS);
        switch (c) {
            case ' ':
            case '\t':
                goto scan;
            case '(':
                return token(LParen);
            case ')':
                return token(RParen);
            case '{':
                return token(LBrace);
            case '}':
                return token(RBrace);
            case '[':
                return token(LBrack);
            case ']':
                return token(RBrack);
            case ',':
                return token(Comma);
            case '.': {
                if ('.' != next()) {
                    undo();
                    return token(OpDot);
                }
                if ('.' != next()) {
                    undo();
                    goto scan;
                }
                return token(VaArg);
            }
            case '%':
                return token(OpMod);
            case '^':
                return token(OpBitXor);
            case '~':
                return token(OpBitNot);
            case '?':
                return token(QMark);
            case ':':
                if (next() == ':')
                    return token(OpModScope);
                undo();
                return token(Colon);
            case '+':
                switch (next()) {
                    case '+':
                        return token(OpIncr);
                    case '=':
                        return token(OpPlusAssign);
                    default:
                        return undo(), token(OpPlus);
                }
            case '-':
                switch (next()) {
                    case '-':
                        return token(OpDecr);
                    case '=':
                        return token(OpMinusAssign);
                    default:
                        return undo(), token(OpMinus);
                }
            case '*':
                switch (next()) {
                    case '=':
                        return token(OpMulAssign);
                    default:
                        return undo(), token(OpMul);
                }
            case '/':
                switch (next()) {
                    case '=':
                        return token(OpDivAssign);
                    case '/':
                        while (tryNext(c) && (c != '\n' && c));
                        undo();
                        goto scan;
                    case '*':
                        while (tryNext(c) && (!(c == '/' && source[offset - 2] == '*'))) {
                            if (c == '\n' || c == '\r')
                                newLine();
                        }
                        goto scan;
                    default:
                        undo();
                        return token(OpDiv);
                }
            case '!':
                return '=' == next()
                       ? token(OpNEq)
                       : (undo(), token(OpNot));
            case '=':
                return '=' == next()
                       ? token(OpEq)
                       : (undo(), token(OpAssign));
            case '&':
                switch (next()) {
                    case '&':
                        return '=' == next()
                               ? token(OpAndAssign)
                               : (undo(), token(OpAnd));
                    default:
                        return undo(), token(OpBitAnd);
                }
            case '|':
                switch (next()) {
                    case '|':
                        return '=' == next()
                               ? token(OpOrAssign)
                               : (undo(), token(OpOr));
                    default:
                        return undo(), token(OpBitOr);
                }
            case '<':
                switch (next()) {
                    case '=':
                        return token(OpLTE);
                    case '<':
                        return token(OpBitShL);
                    default:
                        return undo(), token(OpLT);
                }
            case '>':
                switch (next()) {
                    case '=':
                        return token(OpGTE);
                    case '>':
                        return token(OpBitShR);
                    default:
                        return undo(), token(OpGT);
                }
            case ';':
                return token(Semicolon);
            case '\n':
            case '\r':
                if (need_semi(tok.type)) {
                    return undo(), token(Semicolon);
                }
                newLine();
                goto scan;
            case '"':
                return scan_string();
            case '\'': {
                tok.value = (int64_t) scan_char(next());
                next();
                return token(Char);
            }
            case 0:
                return token(EOS);
            default:
                if (isalpha(c) || '_' == c) return scan_ident(c);
                if (isdigit(c) || '.' == c) {
                    return scan_number(c);
                }
                token(Illegal);
                throw LexerException(this, "illegal character");
        }
    }

/*
 * Inspect the given `tok`, outputting debug information to the stdout.
 */

    void Lexer::inspect() const {
        coloredPrintf(Color::Gray, " %s", TokenHelper::getTokenTypeString(tok.type));

        std::visit([this](auto &&arg) {
            using V = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<V, std::string>)
                coloredPrintf(tok.type == TokenType::Id ? Color::White : Color::Green, " %s", arg.c_str());
            else if constexpr (std::is_same_v<V, float>)
                coloredPrintf(Color::Cyan, " %f", arg);
            else if constexpr (std::is_same_v<V, int>)
                coloredPrintf(Color::Cyan, " %d", arg);
        }, tok.value);

        printf("\n");
    }

    void Lexer::newLine() {
        linepos = 1;
        ++lineno;
    }

    int Lexer::next() {
        ++linepos;
        if (offset + 1 > source.size())
            throw LexerException(this, "Out of bounds.");
        stash = source[offset++];
        return stash;
    }

    int Lexer::undo() {
        --linepos;
        if (offset == 0)
            throw LexerException(this, "Out of bounds.");
        return source[--offset] = stash;
    }

    bool Lexer::tryNext(int &result) {
        if (offset + 1 > source.size())
            return false;

        ++linepos;
        stash = source[offset++];
        result = stash;
        return true;
    }

    bool Lexer::tryUndo(int &result) {
        if (offset == 0)
            return false;
        --linepos;
        source[--offset] = stash;
        result = stash;
        return true;
    }
}
