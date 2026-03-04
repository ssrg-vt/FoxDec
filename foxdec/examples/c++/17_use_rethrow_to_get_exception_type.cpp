#include <iostream>
#include <exception>

    class E1 : public std::exception {};
    class E2 : public std::exception {};

    int main() {
        try {
            throw E2();
        }
        catch( ... ) {
            try {
                throw;
            }
            catch( const E1 & e ) {
                std::cout << "E1\n";
            }
            catch( const E2 & e ) {
                std::cout << "E2\n";
            }
        }
    }
