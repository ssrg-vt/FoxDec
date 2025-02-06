#include <stdio.h>
#include <stdlib.h>
#include <time.h>

const double values [] = {
	1.57079632679489661926e+00, 0x3fff, 0xc90fdaa2L, 0x2168c235L,
	1.125L, 0x3fff, 0x90000000L, 3.142857,
	0.00000000000000000000e+00, 0x0000, 0x00000000L, 1.0,
	9.99999940395355224609e-01, -9.99999940395355224609e-01,
	2.00000013298644055914e-17, 1.99999996755031804853e-17,
	-2.00416836000897277800e-292, 
	-2.22507385850720138309e-308, -4.94065645841246544177e-324,
	1.57079632679489661926e+00, 0x3fff, 0xc90fdaa2L, 0x2168c235L,
	-1.0, -0.5, 0.5,
};

const int nvalues = 26;

union my_union {
	int as_int[2];
	double as_double;
};

double generate_random_value() {
	int n = ((int) rand()) % 5;
	if (n==0) {
		int m = ((int) rand()) % nvalues;
		return values[m];
	}
	else {
		union my_union u;
		u.as_int[0] = rand();
		u.as_int[1] = rand();
		return u.as_double;
	}
}

int main(int argc, char* argv[]) {
	srand(time(NULL));
	for(int i=0;i<1000;i++) {
		int n = ((int) rand()) % 2;
		if (n==0) {
			printf("%lf\n", generate_random_value());
		}
		else if (n==1) {
			printf("%lf %lf\n", generate_random_value(), generate_random_value());
		}
	}
}
