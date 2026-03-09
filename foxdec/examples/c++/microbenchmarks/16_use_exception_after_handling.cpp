#include <iostream>

struct E {
  const char* message;
  E() : message("Class E") { }

  ~E() { std::puts("~E()"); }
};


int main(void) {
	try {
 	 throw E{};
	} catch (E& e) {
 	 try {
 	   throw;
 	 } catch (E& e) {
 	   std::cout << "Exception: " << e.message << std::endl;
 	   // end_catch #1
 	 }
 	 std::cout << "Exception: " << e.message << std::endl;
	  // end_catch #2
	}
}

