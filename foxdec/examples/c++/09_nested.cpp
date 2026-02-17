#include <exception>
#include <iostream>
#include <stdexcept>
#include <string>

// Helper: recursively print nested exceptions
void print_exception(const std::exception& e, int level = 0)
{
    std::cerr << std::string(level * 2, ' ')
              << "Exception: " << e.what() << '\n';

    try {
        std::rethrow_if_nested(e);
    } catch (const std::exception& nested) {
        print_exception(nested, level + 1);
    } catch (...) {
        std::cerr << std::string((level + 1) * 2, ' ')
                  << "Unknown nested exception\n";
    }
}

// Lowest-level failure
void low_level()
{
    throw std::runtime_error("Disk read failed");
}

// Middle layer: adds context, does NOT catch permanently
void mid_level()
{
    try {
        low_level();
    } catch (...) {
        std::throw_with_nested(
            std::runtime_error("Failed to load configuration")
        );
    }
}

// Top layer: adds even more context
void high_level()
{
    try {
        mid_level();
    } catch (...) {
        std::throw_with_nested(
            std::runtime_error("Application startup failed")
        );
    }
}

int main()
{
    try {
        high_level();
    } catch (const std::exception& e) {
        std::cerr << "Caught exception chain:\n";
        print_exception(e);
    }
}
