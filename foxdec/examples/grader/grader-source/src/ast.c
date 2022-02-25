#include "ast.h"
#include <stdio.h>
#include <stdlib.h>

#define match(X) list[*pos].type == (X)

Expr equality(Token* list, size_t* pos);
Expr term(Token* list, size_t* pos);
Expr factor(Token* list, size_t* pos);
Expr primary(Token* list, size_t* pos);


int is_new_line(Token* list, size_t* pos) {
    if (match(NEW_LINE)) {
        *pos += 1;
        return 1;
    }
    return 0;
}

Expr* parse(Token* list) {
    size_t pos = 0;
    size_t maximumExprListSize = 128;
    size_t exprListSize = 0;
    Expr* result = malloc(sizeof(Expr)*maximumExprListSize);
    do {
        if (exprListSize == maximumExprListSize) {
            maximumExprListSize += 128;
            result = realloc(result, maximumExprListSize);
        }
        result[exprListSize++] = equality(list, &pos);
    } while (is_new_line(list, &pos));
    if (exprListSize == maximumExprListSize) {
        maximumExprListSize += 1;
        result = realloc(result, maximumExprListSize);
    }
    // result[exprListSize].left = NULL;
    // result[exprListSize].right = NULL;
    // result[exprListSize].op.type = TEOF;
    return result;
}

char* to_string(Expr expr) {
    char buffer[256];
    char* l = expr.left ? to_string(*expr.left) : "NULL";
    char* r = expr.right ? to_string(*expr.right) : "NULL";
    int len = 0;
    if (expr.op.type == NUMBER) {
        len = sprintf(buffer, "%f", expr.op.literal);
    } else if (expr.op.type == FUNCTION) {
        len = sprintf(buffer, "%s(%s)", expr.op.token, l);
    } else if (expr.op.type == LEFT_PAREN) {
        len = sprintf(buffer, "(%s)", l);
    } else {
        len = sprintf(buffer, "(%s %s %s)", l, expr.op.token ? expr.op.token : "NULL", r);
    }
    if (expr.left)
        free(l);
    if (expr.right)
        free(r);
    char *result = malloc(len+1);
    strncpy(result, buffer, len+1);
    return result;
}


Expr equality(Token* list, size_t* pos) {
    Expr expr = term(list, pos);

    while(match(EQUALS)) {
        *pos += 1;
        Token opeartor = list[*pos-1];
        Expr right = term(list, pos);
        Expr* tmp = malloc(sizeof(Expr));
        *tmp = expr;
        expr.left = tmp;
        tmp = malloc(sizeof(Expr));
        *tmp = right;
        expr.op = opeartor;
        expr.right = tmp;
    }

    return expr;
}

Expr term(Token* list, size_t* pos) {
    Expr expr = factor(list, pos);

    while (match(MINUS) || match(PLUS)) {
        *pos += 1;
        Token opeartor = list[*pos-1];
        Expr right = factor(list, pos);
        Expr* tmp = malloc(sizeof(Expr));
        *tmp = expr;
        expr.left = tmp;
        tmp = malloc(sizeof(Expr));
        *tmp = right;
        expr.op = opeartor;
        expr.right = tmp;
    }

    return expr;
}

Expr factor(Token* list, size_t* pos) {
    Expr expr = primary(list, pos);

    while (match(MULTIPLY) || match(DIVIDE)) {
        *pos += 1;
        Token opeartor = list[*pos-1];
        Expr right = primary(list, pos);
        Expr* tmp = malloc(sizeof(Expr));
        *tmp = expr;
        expr.left = tmp;
        tmp = malloc(sizeof(Expr));
        *tmp = right;
        expr.op = opeartor;
        expr.right = tmp;
    }

    return expr;
}

Expr primary(Token* list, size_t* pos) {
    if (match(NUMBER) || match(IDENTIFIER)) {
        *pos += 1;
        Expr result;
        result.left = NULL;
        result.right = NULL;
        result.op = list[*pos-1];
        return result;
    }

    size_t paren_pos = *pos;
    if (match(FUNCTION)) {
        *pos += 1;
    }

    if (match(LEFT_PAREN)) {
        *pos += 1;
        Expr expr = term(list, pos);
        if (!match(RIGHT_PAREN)) {
            fprintf(stderr, "Expected ')' after expression");
        } else {
            *pos += 1;
        }
        Expr* tmp = malloc(sizeof(Expr));
        *tmp = expr;
        expr.left = tmp;
        expr.op = list[paren_pos];
        expr.right = NULL;
        return expr;
    }

    if (!match(TEOF)) {
        Token* token = &list[*pos];
        fprintf(stderr, "Unexpected ");
        print_token(token, stderr);
        fprintf(stderr, "\n");
    }
    Expr expr;
    expr.left = NULL;
    expr.op.type = TEOF;
    expr.right = NULL;
    return expr;
}