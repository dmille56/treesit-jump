#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function Declaration
int add(int, int);
void printMessage(void);

// Struct Declaration
typedef struct {
    char name[50];
    int age;
} Person;

// Union Declaration
union Data {
    int i;
    float f;
    char str[20];
};

// Enum Declaration
enum boolean {
    NO,
    YES
};

int main() {
    // Variable Declarations
    int result;
    char* dynamicString;
    Person person1;
    union Data data;
    enum boolean isTrue;

    // Using the function
    result = add(5, 3);
    printf("Result of add: %d\n", result);

    // Dynamic Memory Allocation
    dynamicString = (char*)malloc(20 * sizeof(char));
    strcpy(dynamicString, "C Programming");
    printf("Dynamic String: %s\n", dynamicString);
    free(dynamicString);

    // Struct Usage
    strcpy(person1.name, "John Doe");
    person1.age = 30;
    printf("Person: %s, %d\n", person1.name, person1.age);

    // Union Usage
    data.i = 10;
    printf("Data.i: %d\n", data.i);
    data.f = 220.5;
    printf("Data.f: %.1f\n", data.f);
    strcpy(data.str, "C Programming");
    printf("Data.str: %s\n", data.str);

    // Enum Usage
    isTrue = YES;
    printf("Is True? %d\n", isTrue);

    // Pointers
    int* ptr = &result;
    printf("Pointer to result: %d\n", *ptr);

    // Arrays
    int numbers[5] = {1, 2, 3, 4, 5};
    printf("Array element: %d\n", numbers[2]);

    // Conditional Statement
    if (result > 0) {
        printMessage();
    }

    // Loop
    for(int i = 0; i < 5; i++) {
        printf("%d ", numbers[i]);
    }
    printf("\n");

    return 0;
}

// Function Implementation
int add(int a, int b) {
    return a + b;
}

void printMessage(void) {
    printf("Result is positive\n");
}
