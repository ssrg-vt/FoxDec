#ifndef __GRADER_LEXER_H__
#define __GRADER_LEXER_H__

#include <string.h>
#include <stdio.h>

#define NAN __builtin_nanf ("")

typedef enum eTokenType {
    LEFT_PAREN, RIGHT_PAREN, 
    MINUS, PLUS, DIVIDE, MULTIPLY,
    EQUALS, NUMBER, FUNCTION, IDENTIFIER,

    NEW_LINE, TEOF
} TokenType;

typedef struct sToken {
    TokenType type;
    char* token;
    float literal;
    size_t line_number;
} Token;

void print_token(Token* token, FILE* out);
Token* lex(FILE *fp);

#endif