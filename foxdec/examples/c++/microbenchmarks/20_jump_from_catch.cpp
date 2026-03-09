#include <iostream>
#include <fstream>
#include <vector>
#include <stdexcept>

double safeDivide(double a, double b) {
    if (b == 0.0) {
        throw std::runtime_error("Division by zero");
    }
    return a / b;
}

std::vector<double> readNumbersFromFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file: " + filename);
    }

    std::vector<double> numbers;
    double value;

    while (file >> value) {
        numbers.push_back(value);
    }

    if (numbers.empty()) {
        throw std::runtime_error("No valid numbers found in file");
    }

    return numbers;
}

int main() {
    try {
        std::vector<double> numbers = readNumbersFromFile("data.txt");

        try {
            double result = safeDivide(numbers.at(0), numbers.at(1));
            std::cout << "Result: " << result << std::endl;
        }
        catch (const std::out_of_range& e) {
            std::cerr << "Not enough numbers in the file: " << e.what() << std::endl;
        }
        catch (const std::runtime_error& e) {
            std::cerr << "Math error: " << e.what() << std::endl;
goto label;
        }

    }
    catch (const std::runtime_error& e) {
        std::cerr << "Runtime error: " << e.what() << std::endl;
    }
    catch (const std::exception& e) {
        std::cerr << "Unexpected error: " << e.what() << std::endl;
    }
    std::cout << "Program finished." << std::endl;
label:
    return 0;
}
