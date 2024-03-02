// Variables and Data Types
let name = 'John Doe'; // String
const age = 30; // Number
var isDeveloper = true; // Boolean
let skills = ['JavaScript', 'React', 'Node.js']; // Array
let person = { // Object
    name: name,
    age: age,
    skills: skills
};

// Functions
function greet(name) {
    return `Hello, ${name}!`;
}

const sayGoodbye = function(name) {
    return `Goodbye, ${name}!`;
};

const getAge = (birthYear) => new Date().getFullYear() - birthYear;

// Classes and Inheritance
class Person {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }

    greet() {
        return `Hello, my name is ${this.name}.`;
    }
}

class Employee extends Person {
    constructor(name, age, position) {
        super(name, age);
        this.position = position;
    }

    work() {
        return `${this.name} is working as a ${this.position}.`;
    }
}

const employee = new Employee('Jane Doe', 28, 'Web Developer');
console.log(employee.greet());
console.log(employee.work());

// Promises and Async/Await
const fetchData = () => {
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve("Data fetched"), 2000);
    });
};

fetchData().then(data => console.log(data));

const asyncFunction = async () => {
    const data = await fetchData();
    console.log(data);
};

asyncFunction();

// Template Literals
const user = 'John';
const greeting = `Welcome back, ${user}!`;
console.log(greeting);

// Destructuring
const userInfo = { userName: 'JohnDoe', userAge: 30 };
const { userName, userAge } = userInfo;
console.log(userName); // JohnDoe

const numbers = [1, 2, 3, 4, 5];
const [first, second] = numbers;
console.log(first); // 1

// Spread and Rest Operators
const arr1 = [1, 2, 3];
const arr2 = [...arr1, 4, 5];
console.log(arr2); // [1, 2, 3, 4, 5]

function sum(...numbers) {
    return numbers.reduce((acc, current) => acc + current, 0);
}
console.log(sum(1, 2, 3)); // 6

// "Modules" in a single file context - Functions as an example
const mathOperations = {
    add: (a, b) => a + b,
    subtract: (a, b) => a - b,
};

console.log(mathOperations.add(5, 3)); // 8
console.log(mathOperations.subtract(10, 5)); // 5
