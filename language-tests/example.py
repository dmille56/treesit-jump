import functools

# Variable Declarations
name = "John Doe"
age = 30
is_programmer = True
skills = ["Python", "Django", "Flask"]

# Function Definition
def greet(name):
    return f"Hello, {name}!"

# Lambda Function
square = lambda x: x ** 2

# List Comprehension
squared_numbers = [x ** 2 for x in range(10)]

# Dictionary Comprehension
squared_dict = {x: x ** 2 for x in range(5)}

# Generator Expression
sum_of_squares = sum(x ** 2 for x in range(10))

# Decorator
def debug(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        result = func(*args, **kwargs)
        print(f"{func.__name__} returned {result}")
        return result
    return wrapper

@debug
def add(a, b):
    return a + b

# Class Definition
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age
    
    def __str__(self):
        return f"{self.name}, {self.age}"

# Inheritance
class Employee(Person):
    def __init__(self, name, age, position):
        super().__init__(name, age)
        self.position = position
    
    def __str__(self):
        return f"{super().__str__()}, {self.position}"

# Exception Handling
try:
    print(10 / 0)
except ZeroDivisionError:
    print("Cannot divide by zero")

# File I/O
with open("test.txt", "w") as f:
    f.write("Hello, Python!")

# Context Manager
class OpenFile:
    def __init__(self, filename, mode):
        self.filename = filename
        self.mode = mode
    
    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

with OpenFile("test.txt", "r") as f:
    content = f.read()
    print(content)

# Using Everything
if __name__ == "__main__":
    print(greet(name))
    print(square(4))
    print(squared_numbers)
    print(squared_dict)
    print(sum_of_squares)
    print(add(2, 3))
    person = Person("Jane Doe", 28)
    employee = Employee("John Smith", 35, "Developer")
    print(person)
    print(employee)
