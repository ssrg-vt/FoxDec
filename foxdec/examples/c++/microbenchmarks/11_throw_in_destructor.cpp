#include <iostream>
using namespace std;
class A {
 public:
  ~A() noexcept(false) {
    try {
      printf("exception in A start\n");
      throw 30;
      printf("exception in A end\n");      
    }catch(int e) {
      printf("catch in A %d\n",e);
    }
  }
};
class B{
 public:
  ~B() noexcept(false) {
    printf("exception in B start\n");
    throw 20;
    printf("exception in B end\n");    
  }
};
int main(void) {
  try {
    A a;
    B b;
  }catch(int e) {
    printf("catch in main %d\n",e);
  }
  return 0;
}
