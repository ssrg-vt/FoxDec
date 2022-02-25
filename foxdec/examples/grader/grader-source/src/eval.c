#include "eval.h"
#include <stdio.h>

#define sin(n) (n) - ((n)*(n)*(n))/6 + ((n)*(n)*(n)*(n)*(n))/120 - ((n)*(n)*(n)*(n)*(n)*(n)*(n))/5040 + ((n)*(n)*(n)*(n)*(n)*(n)*(n)*(n)*(n))/362880
#define cos(n) 1.0 - ((n)*(n))/2 + ((n)*(n)*(n)*(n))/24 - ((n)*(n)*(n)*(n)*(n)*(n))/720 + ((n)*(n)*(n)*(n)*(n)*(n)*(n)*(n))/40320

float evaluate(Expr expr) {
    float left, right;
    if (expr.left) {
        left = evaluate(*expr.left);
    }
    if (expr.right) {
        right = evaluate(*expr.right);
    }
    switch(expr.op.type) {
        case LEFT_PAREN: return left;
        case MINUS: return left - right;
        case PLUS: return left + right;
        case DIVIDE: return left / right;
        case MULTIPLY: return left * right;
        case EQUALS: return (float) (left == right);
        case NUMBER:  return expr.op.literal;
        case FUNCTION: 
            if (strcmp("sin", expr.op.token) == 0) return sin(left); 
            if (strcmp("cos", expr.op.token) == 0) return cos(left);
            fprintf(stderr, "Function %s is not implemented\n", expr.op.token); return NAN;
    }
}