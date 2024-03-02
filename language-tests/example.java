import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaFeaturesDemo {

    // Method Overloading
    public static int add(int a, int b) {
        return a + b;
    }

    public static double add(double a, double b) {
        return a + b;
    }

    // Inner Class
    static class Person {
        String name;
        int age;

        Person(String name, int age) {
            this.name = name;
            this.age = age;
        }

        void introduce() {
            System.out.println("Hello, my name is " + name + " and I am " + age + " years old.");
        }
    }

    // Inheritance
    static class Employee extends Person {
        String position;

        Employee(String name, int age, String position) {
            super(name, age);
            this.position = position;
        }

        @Override
        void introduce() {
            super.introduce();
            System.out.println("I work as a " + position + ".");
        }
    }

    // Lambda Expressions and Stream API
    public static void processPersons(ArrayList<Person> persons, Function<Person, String> function) {
        for (Person person : persons) {
            System.out.println(function.apply(person));
        }
    }

    public static void main(String[] args) {
        // Using Method Overloading
        System.out.println(add(5, 3));
        System.out.println(add(5.5, 3.3));

        // Using Inner Class
        Person person = new Person("John Doe", 30);
        person.introduce();

        // Using Inheritance
        Employee employee = new Employee("Jane Doe", 28, "Software Developer");
        employee.introduce();

        // Using ArrayList and HashMap
        ArrayList<Person> persons = new ArrayList<>();
        persons.add(new Person("Alice", 24));
        persons.add(new Employee("Bob", 26, "Designer"));

        HashMap<String, Integer> ageMap = new HashMap<>();
        for (Person p : persons) {
            ageMap.put(p.name, p.age);
        }
        System.out.println(ageMap);

        // Using Lambda Expressions and Stream API
        processPersons(persons, p -> p.name + " is " + p.age + " years old.");

        // Filtering and Collecting with Streams
        String names = persons.stream()
                .filter(p -> p.age > 25)
                .map(p -> p.name)
                .collect(Collectors.joining(", "));
        System.out.println("Persons older than 25: " + names);
    }
}
