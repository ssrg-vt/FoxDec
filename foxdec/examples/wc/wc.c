#include <stdio.h>
#include <unistd.h>
#include <locale.h>
#include <runetype.h>
#include <ctype.h>


extern int main2(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  printf("%lu\n", sizeof(_RuneLocale));
  printf("%.8s\n", _DefaultRuneLocale.__magic);
  main2(argc,argv);
  return 0;
}

