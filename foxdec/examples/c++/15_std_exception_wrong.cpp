#include <string>
#include <exception>
#include <iostream>


/*
 * This example misses "public", meaning that MyException is not visibly a subclass of std::exception.
 * As a result, this exception is not caught.
 * */

class MyException: /* public */ std::exception
{
public:
    explicit MyException(const char* message)
        : msg_(message) {}

    virtual const char* what() const noexcept {
       return msg_.c_str();
    }

protected:
    /** Error message.
     */
    std::string msg_;
};

int main()
{
    try
    {
        throw MyException("MESSAGE");
    }
    catch (const std::exception & ex)
    {
        std::cout << ex.what() << std::endl;
    }


    return 0;
}
