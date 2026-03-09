#include <stdio.h>
#include <stdlib.h>


#include <iostream>
#include <fstream>
#include <stdexcept>






int main() {
    /* Array of label addresses (GCC/Clang extension) */
    void *labels[] = {
        &&label1
    };

    try {
        std::cout << "Program is running" << std::endl;
        throw(std::runtime_error("Hello"));
    }
    catch (const std::runtime_error& e) {
    goto *labels[0];  // indirect goto
        std::cerr << "Runtime error: " << e.what() << std::endl;
    }
    catch (...) {
label1:
    	std::exception_ptr p = std::current_exception();
    	std::cout << "Exception type: " << (p ? p.__cxa_exception_type()->name() : "null") << std::endl;
    }


    std::cout << "Program finished." << std::endl;
    return 0;
}
