// CircleSquare.go
package main

import (
	"fmt"
	"math"
)

func main() {
	var r float64
	fmt.Print("Введите радиус круга: ")
	_, err := fmt.Scan(&r)
	if err != nil {
		fmt.Println("Ошибка ввода:", err)
		return
	}
	area := math.Pi * r * r
	fmt.Printf("Площадь круга: %.6f\n", area)
}