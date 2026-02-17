#include <iostream>
#include <stdexcept>
#include <string>

void mayThrow(int choice) {
    switch (choice) {
        case 1:
            throw std::runtime_error("Runtime error occurred!");
        case 2:
            throw std::out_of_range("Out of range error!");
        case 3:
            throw std::invalid_argument("Invalid argument error!");
        case 4:
            throw 42; // Throwing an int
        default:
            throw std::string("Unknown exception type!");
    }
}

int main() {
    for (int i = 1; i <= 5; i++) {
        try {
            std::cout << "\nTest case " << i << ":\n";
            mayThrow(i);
        }
        catch (const std::out_of_range& e) {
            std::cout << "Caught out_of_range: " << e.what() << "\n";
        }
        catch (const std::runtime_error& e) {
            std::cout << "Caught runtime_error: " << e.what() << "\n";
        }
        catch (const std::invalid_argument& e) {
            std::cout << "Caught invalid_argument: " << e.what() << "\n";
        }
        catch (int n) {
            std::cout << "Caught an int: " << n << "\n";
        }
        catch (const std::string& s) {
            std::cout << "Caught string: " << s << "\n";
        }
        catch (...) {
            std::cout << "Caught an unknown exception!\n";
        }
    }

    return 0;
}
