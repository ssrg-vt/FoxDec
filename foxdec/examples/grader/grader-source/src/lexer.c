#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>

void print_token(Token* token, FILE* out) {
    char* token_name;
    #define TOKEN_NAME(type) case (type): token_name = #type; break
    switch(token->type) {
        TOKEN_NAME(LEFT_PAREN);
        TOKEN_NAME(RIGHT_PAREN);
        TOKEN_NAME(MINUS);
        TOKEN_NAME(PLUS);
        TOKEN_NAME(DIVIDE);
        TOKEN_NAME(MULTIPLY);
        TOKEN_NAME(EQUALS);
        TOKEN_NAME(NUMBER);
        TOKEN_NAME(FUNCTION);
        TOKEN_NAME(IDENTIFIER);
        TOKEN_NAME(NEW_LINE);
        TOKEN_NAME(TEOF);
    }
    #undef TOKEN_NAME
    fprintf(out, "Token %s on line %ld: \"%s\" %f", token_name, token->line_number,  token->token, token->literal);
}

Token* lex(FILE *fp) {
    size_t maximumTokenListLength = 128;
    size_t maximumTokenLength = 128;

    Token* tokenList = (Token*)malloc(sizeof(Token) * maximumTokenListLength);
    int ch;
    size_t tokenSize = 0;
    size_t tokenListSize = 0;
    Token token;
    token.type = TEOF;
    TokenType newTokenType;
    size_t line_number = 1;

    while (1) {
        ch = getc(fp);
        // printf("%c", ch);
        if (ch == '\n') {
            line_number++;
            newTokenType = NEW_LINE;
        } else if ((ch >= '(' && ch <= '=') || (ch >= 'A' && ch <= 'z')) {
            switch(ch) {
                case '(': newTokenType = LEFT_PAREN; break;
                case ')': newTokenType = RIGHT_PAREN; break;
                case '-': newTokenType = MINUS; break;
                case '+': newTokenType = PLUS; break;
                case '/': newTokenType = DIVIDE; break;
                case '*': newTokenType = MULTIPLY; break;
                case '=': newTokenType = EQUALS; break;
                case '.': if (token.type == NUMBER) {
                    newTokenType = NUMBER;
                } else {
                    continue;
                } break;
                default: if (ch >= '0' && ch <='9') {
                    newTokenType = NUMBER;
                } else if (ch >= 'A' && ch <= 'z') {
                    newTokenType = IDENTIFIER;
                } else {
                    continue;
                }
            }
        } else if (ch == EOF) {
            newTokenType = TEOF;
        } else {
            continue;
        }
        // We parsed a valid new token (or long token)
        // printf("New token type: %d\n", (int) newTokenType);
        if (newTokenType != token.type || (newTokenType != NUMBER && newTokenType != IDENTIFIER)) {
            if (newTokenType == LEFT_PAREN && token.type == IDENTIFIER) {
                token.type = FUNCTION;
            }
            if (token.type == NUMBER) {
                token.literal = atof(token.token);
            } else {
                token.literal = NAN;
            }
            if (tokenListSize == maximumTokenListLength) {
                maximumTokenListLength += 128;
                tokenList = realloc(tokenList, maximumTokenListLength);
            }
            if (token.type != TEOF) {
                tokenList[tokenListSize++] = token;
                // print_token(&token);
            }
            token.type = newTokenType;
            token.line_number = line_number;
            tokenSize = 0;
            if (newTokenType == TEOF) {
                token.token = NULL;
                if (tokenListSize == maximumTokenListLength) {
                    maximumTokenListLength += 1;
                    tokenList = realloc(tokenList, maximumTokenListLength);
                }
                tokenList[tokenListSize++] = token;
                return tokenList;
            } else if (newTokenType == NUMBER || newTokenType == FUNCTION) {
                maximumTokenLength = 128;
                token.token = (char*)malloc(sizeof(char) * maximumTokenLength);
                token.token[tokenSize++] = ch;
            } else {
                token.token = (char*) malloc(sizeof(char)*2);
                token.token[tokenSize++] = ch; 
            }
        } else { // this is a second consecutive NUMBER or FUNCTION
            if (tokenSize == maximumTokenLength) {
                maximumTokenLength += 128;
                token.token = realloc(token.token, maximumTokenLength);
            }
            token.token[tokenSize++] = ch;
        }
        token.token[tokenSize] = '\0';
    }


}