#include <iostream>
#include <exception>
#include <stdexcept>


class E1 {
  public:
	 ~E1() { std::cout << "Destructing E1."; }
};

int main() {
try {
    throw E1(); // std::runtime_error("Something went wrong");
}
catch (...) {
    try {
        throw;  // rethrow current exception
    }
    catch (const std::runtime_error& e) {
        std::cout << "runtime_error: " << e.what() << "\n";
    }
    catch (const std::exception& e) {
        std::cout << "std::exception: " << e.what() << "\n";
    }
    catch (...) {
        std::cout << "Unknown non-standard exception\n";
    }
    std::cout << "After inner catch.\n";
    std::exception_ptr p = std::current_exception(); 
    std::cout << "Exception type: " << (p ? p.__cxa_exception_type()->name() : "null") << std::endl;
}
}
