
# Примеры программ на Glacier

## 1. SumNumbers

**Описание**: складывает два числа, введённых пользователем.

```glacier
// SumNumbers
let a: int = readNumber()
let b: int = readNumber()
let sum: int = a + b
print(sum)
```

---

## 2. GeometricMean

**Описание**: вычисляет среднее геометрическое двух чисел.
(Используется возведение в степень через оператор `**`.)

```glacier
// GeometricMean
let x: float = readNumber()
let y: float = readNumber()
let mean: float = (x * y) ** 0.5
print(mean)
```

---

## 3. CircleSquare

**Описание**: вычисляет площадь круга по радиусу.

```glacier
// CircleSquare
let r: float = readNumber()
let area: float = Pi * r ** 2
print(area)
```
