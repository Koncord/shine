#pragma once

#include <cassert>
#include <string>
#include <variant>

namespace shine
{

#define SHINE_TOKEN_LIST \
  t(Illegal, "illegal") \
  t(EOS, "end-of-source") \
  t(Id, "id") \
  t(I64, "i64") \
  t(I32, "i32") \
  t(I16, "i16") \
  t(I8, "u8") \
  t(U64, "u64") \
  t(U32, "u32") \
  t(U16, "u16") \
  t(U8, "u8") \
  t(VaArg, "...") \
  t(Float, "float") \
  t(String, "string") \
  t(Boolean, "boolean") \
  t(True, "true") \
  t(False, "false") \
  t(Extern, "extern") \
  t(Def, "def") \
  t(Struct, "struct") \
  t(Use, "use") \
  t(As, "as") \
  t(While, "while") \
  t(Until, "until") \
  t(Continue, "continue") \
  t(Break, "break") \
  t(If, "if") \
  t(Unless, "unless") \
  t(Else, "else") \
  t(For, "for") \
  t(Let, "let") \
  t(Const, "const") \
  t(End, "end") \
  t(Return, "return") \
  t(Repeat, "repeat") \
  t(Case, "case") \
  t(When, "when") \
  t(SizeOf, "sizeof") \
  t(LBrace, "{") \
  t(RBrace, "}") \
  t(LParen, "(") \
  t(RParen, ")") \
  t(LBrack, "[") \
  t(RBrack, "]") \
  t(Colon, ":") \
  t(QMark, "?") \
  t(Semicolon, ";") \
  t(Comma, ",") \
  t(OpDot, ".") \
  t(OpLNot, "not") \
  t(OpNot, "!") \
  t(OpPlus, "+") \
  t(OpIncr, "++") \
  t(OpMinus, "-") \
  t(OpDecr, "--") \
  t(OpMul, "*") \
  t(OpDiv, "/") \
  t(OpMod, "%") \
  t(OpGT, ">") \
  t(OpLT, "<") \
  t(OpGTE, ">=") \
  t(OpLTE, "<=") \
  t(OpEq, "==") \
  t(OpNEq, "!=") \
  t(OpAnd, "&&") \
  t(OpOr, "||") \
  t(OpAssign, "=") \
  t(OpPlusAssign, "+=") \
  t(OpMinusAssign, "-=") \
  t(OpMulAssign, "*=") \
  t(OpDivAssign, "/=") \
  t(OpAndAssign, "&=") \
  t(OpOrAssign, "|=") \
  t(OpBitAnd, "&") \
  t(OpBitOr, "|") \
  t(OpBitXor, "^") \
  t(OpBitNot, "~") \
  t(OpBitShL, "<<") \
  t(OpBitShR, ">>") \
  t(Last, "")

    enum class TokenType
    {
#define t(tok, str) tok,
        SHINE_TOKEN_LIST
#undef t
    };

    struct __T
    {
        TokenType type;
        const char *stoken;

        constexpr __T(TokenType type, const char *stoken) : type(type), stoken(stoken) {}
    };

    class TokenHelper
    {
    public:
        static constexpr __T token_strings[] = {
#define t(tok, str) {TokenType::tok, str},
                SHINE_TOKEN_LIST
#undef t
        };

        static constexpr const char *getTokenName(TokenType type, int N = 0)
        {
            if (token_strings[N].type != type)
                return getTokenName(type, ++N);
            return token_strings[N].stoken;
        }

        static const char *getTokenTypeString(TokenType type)
        {
            assert(type < TokenType::Last);
            return getTokenName(type);
        }

        static bool isCompare(TokenType type)
        {
            return type >= TokenType::OpGT && type <= TokenType::OpNEq;
        }
    };

    struct Token
    {
        int len = 0;
        TokenType type = TokenType::Illegal;

        std::variant<std::string, double, int64_t, bool> value;
    };

    inline bool isRelational(TokenType type)
    {
        return type == TokenType::OpLT || type == TokenType::OpLTE
               || type == TokenType::OpGT || type == TokenType::OpGTE;
    }

    inline bool isEquality(TokenType type) { return type == TokenType::OpEq || type == TokenType::OpNEq; }

    inline bool isEqOrRel(TokenType type) { return isRelational(type) || isEquality(type); }
}