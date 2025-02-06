#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <math.h>

const double bessel_max = 10000;

//TODO gamma_r, lgamma_r
void do_tests1(double d0) {
	printf("acos(%lf) = %lf\n", d0, acos(d0));
	printf("acosh(%lf) = %lf\n", d0, acosh(d0));
	printf("asin(%lf) = %lf\n", d0, asin(d0));
	printf("atanh(%lf) = %lf\n", d0, atanh(d0));
	printf("acosh(%lf) = %lf\n", d0, acosh(d0));
	printf("exp(%lf) = %lf\n", d0, exp(d0));
	printf("gamma(%lf) = %lf\n", d0, gamma(d0));
	printf("j0(%lf) = %lf\n", fmod(d0,bessel_max), j0(fmod(d0,bessel_max)));
	printf("j1(%lf) = %lf\n", fmod(d0,bessel_max), j1(fmod(d0,bessel_max)));
	printf("lgamma(%lf) = %lf\n", d0, lgamma(d0));
	printf("log(%lf) = %lf\n", fabs(d0), log(fabs(d0)));
	printf("log10(%lf) = %lf\n", d0, log10(d0));
	printf("sinh(%lf) = %lf\n", d0, sinh(d0));
	printf("sqrt(%lf) = %lf\n", d0, sqrt(d0));
}

void do_tests2(double d0, double d1) {
	printf("atan(%lf,%lf) = %lf\n", d0, d1, atan(fabs(d0 / d1)));
	printf("atan2(%lf,%lf) = %lf\n", d0, d1, atan2(d0,d1));
	printf("fmod(%lf,%lf) = %lf\n", d0, d1, fmod(d0,d1));
	printf("hypot(%lf,%lf) = %lf\n", d0, d1, hypot(d0,d1));
	printf("jn(%d, %lf) = %lf\n", ((int) d0) % 2, d1, jn(((int) d0) % 2,d1));
	printf("pow(%lf,%lf) = %lf\n", d0, d1, pow(d0,d1));
	printf("remainder(%lf,%lf) = %lf\n", d0, d1, remainder(d0,d1));
	printf("scalb(%lf,%lf) = %lf\n", d0, d1, scalb(d0,d1));
}
int main(int argc, char* argv[]) {
	if (argc <2) {
		puts("Provide file name as argument.");
		exit(1);
	}
	FILE* fp = fopen(argv[1], "r");
	if (fp == NULL) {
		printf("Cannot open file: %s.\n", argv[1]);
		exit(1);
	}

	double in[10];
	double out;
	int n;

	char line[1000];
	char* token;
	const char delim[2] = " ";


	while ((fgets(line,1000,fp))!=NULL) {
		n = 0;
		token = strtok(line,delim);
		while (token != NULL) {
			int i = sscanf(token,"%lf", &(in[n]));
			if (i == 1) {
				n++;
				token = strtok(NULL, delim);
			}
			else {
				token=NULL;
			}
		}
		if (n==0 ) {
		}
		else if (n==1) {
			do_tests1(in[0]);
		}
		else if (n==2) {
			do_tests2(in[0], in[1]);
		}
		else {
			printf("Unknown number of inputs: %d\n", n);
		}
	}
	fclose(fp);
}


