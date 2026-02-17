#include <iostream>

struct X {
    ~X() {
        throw 1; // during unwinding
    }
};

int main() {
    try {
        X x;
        throw 2;
    } catch (...) {
        std::cout << "caught\n";
    }
}
