// SumNumbers.go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	sum := 0.0

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		n, err := strconv.ParseFloat(line, 64)
		if err != nil {
			fmt.Println("Ошибка:", err)
			continue
		}
		sum += n
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Ошибка чтения:", err)
	}

	fmt.Printf("Сумма чисел: %.6f\n", sum)
}