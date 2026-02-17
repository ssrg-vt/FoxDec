#include <iostream>

void throw_extern_exception();

static void throw_string() { throw "String Exception"; }

static void throw_runtime_error_on_10(int x) {
  if (x == 10) {
    throw std::runtime_error("10 was hit");
  }

  std::cout << "10 was not hit" << std::endl;
}

static void rethrow_exception() { throw_extern_exception(); }

int main() {
  try {
    throw_string();
  } catch (const char *ex) {
    std::cout << ex << std::endl;
  }

  throw_runtime_error_on_10(9);
  try {
    throw_runtime_error_on_10(10);
  } catch (const std::runtime_error &ex) {
    std::cout << ex.what() << std::endl;
  }

  try {
    rethrow_exception();
  } catch (const char *ex) {
    std::cout << ex << std::endl;
  }
}
