#include "file.h"
#include <stdio.h>

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return -1;
    }
    EvalResult result = fevaluate(argv[1]);
    if (result.name) {
        printf("%s scored %.02f\n", result.name, result.score*100);
    }
}