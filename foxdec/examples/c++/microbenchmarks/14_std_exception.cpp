#include <string>
#include <exception>
#include <iostream>

class MyException: public std::exception
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
    catch (...)
    {
        std::cerr << "Unhandled Exception!" << std::endl;
    }

    return 0;
}
