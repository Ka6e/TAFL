// FizzBuzz.go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		line := scanner.Text()
		n, err := strconv.Atoi(line)
		if err != nil {
			fmt.Println("Ошибка:", err)
			continue
		}
		fmt.Println(fizzBuzz(n))
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Ошибка чтения:", err)
	}
}

func fizzBuzz(n int) string {
	switch {
	case n % 15 == 0:
		return "FizzBuzz"
	case n % 3 == 0:
		return "Fizz"
	case n % 5 == 0:
		return "Buzz"
	default:
		return strconv.Itoa(n)
	}
}