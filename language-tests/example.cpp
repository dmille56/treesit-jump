#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <memory>

// Function Declaration
int add(int a, int b);

// Lambda Function
auto multiply = [](int a, int b) -> int { return a * b; };

// Class Declaration
class Person {
public:
    std::string name;
    int age;
    Person(std::string name, int age) : name(name), age(age) {}
    void greet() const {
        std::cout << "Hello, my name is " << name << " and I am " << age << " years old." << std::endl;
    }
};

// Inheritance
class Employee : public Person {
public:
    std::string position;
    Employee(std::string name, int age, std::string position)
        : Person(name, age), position(position) {}
    void work() const {
        std::cout << name << " is working as a " << position << "." << std::endl;
    }
};

// Template Function
template <typename T>
T max(T a, T b) {
    return (a > b) ? a : b;
}

// Unique Pointer
std::unique_ptr<Person> createPerson(std::string name, int age) {
    return std::make_unique<Person>(name, age);
}

int main() {
    // Using the function
    std::cout << "Add: " << add(5, 3) << std::endl;

    // Using the lambda
    std::cout << "Multiply: " << multiply(4, 5) << std::endl;

    // Vector
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::for_each(numbers.begin(), numbers.end(), [](int n) {
        std::cout << n << " ";
    });
    std::cout << std::endl;

    // Map
    std::map<std::string, int> ages;
    ages["John"] = 30;
    ages["Jane"] = 28;
    for(const auto& pair : ages) {
        std::cout << pair.first << " is " << pair.second << " years old." << std::endl;
    }

    // Using class and inheritance
    Employee employee("John Doe", 25, "Developer");
    employee.greet();
    employee.work();

    // Template function usage
    std::cout << "Max of 10 and 15: " << max(10, 15) << std::endl;

    // Unique pointer
    auto person = createPerson("Jane Doe", 22);
    person->greet();

    return 0;
}

// Function Implementation
int add(int a, int b) {
    return a + b;
}
