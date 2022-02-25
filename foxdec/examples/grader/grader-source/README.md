# Student math auto-grader

## Building

Run `make`. This will create `greader` binary.

## Running 

Provide `eval.input` file to the grader on the command line: `./grader eval.input`

## Input file syntax

Each line must contain simple algebraic expressions with equality statements. Parenthesis are also supported. In addition, `sin()` and `cos()` functions are supported. Alternatively, a line must contain `name = STRING` entry describing student's name. See `eval.input` for sample syntax.
