#include <iostream>
#include <exception>
#include <stdexcept>

struct BadException {
    ~BadException() noexcept(false) {  // explicitly allow throwing
        std::cout << "BadException destructor running...\n";
        throw std::runtime_error("Exception from destructor");
    }
};

int main() {
    try {
      throw BadException{};
    } catch (const std::exception& e) {
        std::cout << "Caught: " << e.what() << "\n";
    }

    std::cout << "End of main\n";
}
