package main

import (
	"fmt"
	"sync"
	"time"
)

// Struct definition
type Employee struct {
	ID        int
	FirstName string
	LastName  string
	Position  string
}

// Interface definition
type Person interface {
	Introduce()
}

// Method implementation for the struct
func (e Employee) Introduce() {
	fmt.Printf("Hi, I'm %s %s, and I work as a %s.\n", e.FirstName, e.LastName, e.Position)
}

// Function with multiple return values
func fullName(firstName string, lastName string) (string, int) {
	full := firstName + " " + lastName
	return full, len(full)
}

// Goroutine function
func count(thing string, wg *sync.WaitGroup) {
	defer wg.Done()
	for i := 1; i <= 5; i++ {
		fmt.Println(i, thing)
		time.Sleep(time.Millisecond * 500) // Sleep to simulate work
	}
}

func main() {
	// Using the struct and method
	employee := Employee{1, "John", "Doe", "Developer"}
	employee.Introduce()

	// Using the interface
	var p Person = Employee{2, "Jane", "Doe", "Manager"}
	p.Introduce()

	// Multiple return values
	name, length := fullName("John", "Doe")
	fmt.Println("Full Name:", name, ", Number of Characters:", length)

	// Goroutines and WaitGroup
	var wg sync.WaitGroup
	wg.Add(2)
	go count("sheep", &wg)
	go count("fish", &wg)
	wg.Wait()

	// Map (similar to dictionaries in other languages)
	ages := make(map[string]int)
	ages["john"] = 30
	ages["jane"] = 29
	for name, age := range ages {
		fmt.Printf("%s is %d years old.\n", name, age)
	}

	// Slices
	s := []int{1, 2, 3}
	s = append(s, 4)
	fmt.Println(s)

	// Channels
	messages := make(chan string, 2)
	messages <- "hello"
	messages <- "world"
	close(messages)
	for msg := range messages {
		fmt.Println(msg)
	}

	// Error handling
	if _, err := fmt.Println("Error handling example"); err != nil {
		fmt.Println("An error occurred:", err)
	}
}