#ifndef __GRADER_FILE_H__
#define __GRADER_FILE_H__

typedef struct _EvalStruct {
    char *name;
    float score;
} EvalResult;

EvalResult fevaluate(char *filename);

#endif