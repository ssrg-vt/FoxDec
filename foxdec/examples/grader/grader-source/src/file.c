#include "file.h"
#include "lexer.h"
#include "ast.h"
#include "eval.h"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

EvalResult fevaluate(char *filepath) {
    EvalResult result;
    result.name = NULL;
    result.score = 0.0;
    FILE *fp = fopen(filepath, "r");
    if (!fp) {
        fprintf(stderr, "Cannot open %s: %s\n", filepath, strerror(errno));
        return result;
    }

    Expr* expr = parse(lex(fp));
    int i=0;
    int total = 0;
    result.name = "Unknown";
    while (expr[i].op.type != TEOF) {
        if (expr[i].op.type == EQUALS && expr[i].left && expr[i].left->op.type == IDENTIFIER && strcmp(expr[i].left->op.token, "name") == 0) {
            result.name = expr[i].right->op.token;
        } else {
            // printf("%s\n", to_string(expr[i]));
            result.score += evaluate(expr[i]);
            total += 1;
        }
        i++;
    }
    // printf("Score: %f, total: %d\n", result.score, total);
    result.score /= total;
    
    return result;
}