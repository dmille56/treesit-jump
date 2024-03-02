using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class Program
{
    // Method Overloading
    static int Add(int a, int b) => a + b;
    static double Add(double a, double b) => a + b;

    // Generic Method
    static T Max<T>(T a, T b) where T : IComparable => a.CompareTo(b) > 0 ? a : b;

    // Lambda Expression
    Func<int, int, int> multiply = (x, y) => x * y;

    // Async Method
    static async Task<string> GetDataAsync()
    {
        await Task.Delay(100); // Simulate asynchronous operation
        return "Data retrieved";
    }

    // Main Method
    static async Task Main(string[] args)
    {
        Console.WriteLine(Add(3, 4));
        Console.WriteLine(Add(3.5, 4.5));

        Console.WriteLine(Max(5, 10));
        Console.WriteLine(Max("apple", "orange"));

        var program = new Program();
        Console.WriteLine(program.multiply(5, 6));

        string data = await GetDataAsync();
        Console.WriteLine(data);

        // LINQ and Anonymous Types
        var numbers = new List<int> { 1, 2, 3, 4, 5 };
        var squaredNumbers = numbers.Select(x => new { Number = x, Square = x * x });
        foreach (var n in squaredNumbers)
        {
            Console.WriteLine($"Number: {n.Number}, Square: {n.Square}");
        }

        // Tuple
        var tuple = (Id: 1, Name: "John Doe");
        Console.WriteLine(tuple.Name);

        // Nullable Types
        int? nullableInt = null;
        if (nullableInt.HasValue)
        {
            Console.WriteLine(nullableInt.Value);
        }
        else
        {
            Console.WriteLine("null");
        }

        // Pattern Matching
        object obj = 10;
        if (obj is int i)
        {
            Console.WriteLine($"Integer: {i}");
        }

        // Record Type
        var person = new Person("Jane", "Doe");
        Console.WriteLine(person);

        // Deconstruction
        var (firstName, lastName) = person;
        Console.WriteLine($"First Name: {firstName}, Last Name: {lastName}");
    }
}

// Record Type
public record Person(string FirstName, string LastName);
