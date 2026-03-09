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

    goto *labels[0];  // indirect goto
    try {
        std::cout << "Program is running" << std::endl;
    }
    catch (const std::runtime_error& e) {
label1:
        std::cerr << "Runtime error: " << e.what() << std::endl;
    }

    std::cout << "Program finished." << std::endl;
    return 0;
}
