#ifndef __GRADER_AST_H__
#define __GRADER_AST_H__
#include "lexer.h"

typedef struct sExpr {
    struct sExpr* left;
    Token op;
    struct sExpr* right;
} Expr;

Expr* parse(Token* list);
char* to_string(Expr expr);

#endif