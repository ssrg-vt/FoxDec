#include <iostream>

struct base
{
    base(){std::cout<<"base\n";}
    ~base(){std::cout<<"~base\n";}
};

struct derive : base
{
    derive(){std::cout<<"derive\n"; throw -1;}
    ~derive(){std::cout<<"~derive\n";}
};

int main()
{
    try{
        derive{};
    }
    catch (...){}
    return 0;
}

/*
 * base
 * derive
 * ~base
 *
 *
 * When an exception is thrown from a constructor, stack unwinding begins, destructors for the object will only be called, if an object creation is successful.
 * The destructor of derive is not executed, Because, it is not created successfully.
 * */
