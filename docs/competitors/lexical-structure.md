# Сравнение лексической структуры Go и Haskell

## Основные различия

### 1. Комментарии
**Go**:
- Однострочные: `// comment`
- Многострочные: `/* comment */`

**Haskell**:
- Однострочные: `-- comment`
- Многострочные: `{- comment -}`

### 2. Идентификаторы
**Go**:
- Чувствителен к регистру
- Приватные имена: начинаются с lowercase
- Публичные имена: начинаются с uppercase
- Unicode-идентификаторы разрешены

**Haskell**:
- Чувствителен к регистру
- Имена типов: начинаются с uppercase
- Имена функций/переменных: начинаются с lowercase
- Символы разрешены в операторах: `+`, `*`, `>>=`

### 3. Блоки кода
**Go**: Фигурные скобки
```go
{
    statement1
    statement2
}
```

**Haskell**: Отступы или ключевые слова
```haskell
do
    statement1
    statement2
```

### 4. Ветвления
**Go**: Явный синтаксис
```go
if condition {
    // block
} else {
    // block
}
```

**Haskell**: Выражения с охраной
```haskell
if condition
    then expression
    else expression
```

### 5. Циклы
**Go**: Традиционные конструкции
```go
for i := 0; i < 10; i++ { ... }
for condition { ... }
for key, value := range collection { ... }
```

**Haskell**: Рекурсия и функции высшего порядка
```haskell
-- Рекурсия
loop n = if n > 0 then loop (n-1) else result

-- Функции высшего порядка
map func list
foldl acc func list
```

### 6. Объявление функций
**Go**: Явное объявление с параметрами
```go
func add(a int, b int) int {
    return a + b
}
```

**Haskell**: Уравнения с образцами
```haskell
add a b = a + b
add :: Int -> Int -> Int  -- Сигнатура типа
```

### 7. Базовые типы данных
**Go**:
- Целые: `int`, `int64`
- С плавающей точкой: `float64`
- Строки: `string` (UTF-8)

**Haskell**:
- Целые: `Int`, `Integer`
- С плавающей точкой: `Double`, `Float`
- Строки: `String` (список Char)

### 8. Операторы
**Go**:
- Арифметические: `+`, `-`, `*`, `/`
- Сравнения: `==`, `!=`, `<`, `>`
- Логические: `&&`, `||`, `!`

**Haskell**:
- Арифметические: `+`, `-`, `*`, `/`
- Сравнения: `==`, `/=`, `<`, `>`
- Логические: `&&`, `||`, `not`
- Пользовательские операторы через объявление

### 9. Составные типы
**Go**:
- Массивы: `[5]int`
- Срезы: `[]int`
- Структуры:
```go
type Point struct {
    X, Y int
}
```

**Haskell**:
- Списки: `[Int]`
- Кортежи: `(Int, String)`
- Алгебраические типы:
```haskell
data Point = Point Int Int
```

### 10. Ввод-вывод
**Go**: Функции из пакетов
```go
import "fmt"
fmt.Print("Hello")
```

**Haskell**: Система ввода-вывода (IO Monad)
```haskell
putStrLn "Hello"
getLine >>= putStrLn
```

## Ключевые различия в лексике
1. **Синтаксис групп**:
   - Go использует фигурные скобки
   - Haskell использует отступы

2. **Объявления переменных**:
   - Go: `var x int = 5` или `x := 5`
   - Haskell: `let x = 5` (в функциях)

3. **Обработка ошибок**:
   - Go: явная проверка error
   - Haskell: монада Either/Maybe

4. **Система типов**:
   - Go: интерфейсы и структурная типизация
   - Haskell: алгебраические типы и полиморфизм






## 1. Примеры кода

```glacier
// Функциональный стиль с императивными элементами
module main

greet :: String -> String
greet name = "Hello, " ++ name

processNumbers :: [Int] -> Int
processNumbers nums =
    let filtered = nums |> filter (> 0)
        doubled = filtered |> map (* 2)
    in foldl (+) 0 doubled

main :: IO ()
main = do
    let names = ["Alice", "Bob", "Charlie"]

    -- Императивный блок с монадическими операциями
    cmd {
        print "Starting program..."

        for name in names do
            when (length name > 3) do
                let message = greet name
                print message

        result <- calculate 5 3
        print $ "Result: " ++ show result
    }

calculate :: Int -> Int -> IO Int
calculate x y = pure $ x * y + x - y
```

## 2. Ключевые слова

| Ключевое слово | Предназначение                          | Источник вдохновения |
| -------------- | --------------------------------------- | -------------------- |
| `module`       | Объявление модуля                       | Haskell              |
| `import`       | Импорт модулей                          | Both                 |
| `func`         | Объявление функции                      | Go                   |
| `let`          | Привязка значений                       | Haskell              |
| `in`           | Область видимости let                   | Haskell              |
| `do`           | Монадические блоки                      | Haskell              |
| `cmd`          | Императивные блоки                      | Glacier unique       |
| `if`/`then`/`else` | Условные выражения                 | Haskell              |
| `case`/`of`    | Сопоставление с образцом                | Haskell              |
| `for`/`in`     | Итерация по коллекциям                  | Go                   |
| `when`         | Условное выполнение                     | Haskell              |
| `pure`         | Завершение монадических вычислений      | Haskell              |
| `type`         | Объявление типа                         | Both                 |
| `interface`    | Объявление интерфейса                   | Go                   |
| `struct`       | Объявление структуры                    | Go                   |
| `where`        | Ограничения типов                       | Haskell              |

## 3. Идентификаторы

**Правила именования:**
- Функции и переменные: `camelCase` (в стиле Go)
- Типы и модули: `PascalCase` (в стиле Go/Haskell)
- Константы: `SCREAMING_SNAKE_CASE` или `camelCase`
- Параметры типов: `singleUppercase` (как в Haskell)

**Примеры:**
```glacier
-- Функции и переменные
calculateTotal, getUserById, isValid

-- Типы
User, Result[T], TreeMap[K,V]

-- Константы
MAX_SIZE, defaultTimeout, pi

-- Параметры типов
T, K, V, E
```

## 4. Литералы

### 4.1. Литералы чисел

**Унифицированный синтаксис:**
```glacier
-- Целые числа (как в Go)
42, 0x2A, 0b1010, 1_000_000

-- Числа с плавающей точкой (как в Haskell)
3.14, 6.022e23, 1.6e-19

-- Rational числа (уникальная фича)
1/3, 22/7, -5/2
```

### 4.2. Литералы строк

**Многообразие строковых литералов:**
```glacier
-- Базовые строки (Go-стиль)
"Hello, World!\n"

-- Строки с интерполяцией (уникально)
"Hello, ${name}! Result: ${calculate 5 3}"

-- Символы (Haskell-стиль)
'a', '\n', '\x263A'

-- Многострочные строки (уникально)
"""
This is a multi-line
string with ${interpolation}
"""

-- Сырые строки (Go-стиль)
`C:\Users\Name\file.txt`

-- Строки без экранирования (уникально)
r#"Raw string with "quotes""#
```

## 5. Операторы

### 5.1. Базовые операторы (Go-стиль)
```glacier
+ - * / %          -- Арифметика
== != < > <= >=    -- Сравнение
&& || !            -- Логические
& | ^ << >>        -- Битовые
```

### 5.2. Функциональные операторы (Haskell-стиль)
```glacier
++                 -- Конкатенация списков
::                 -- Annotation типа
.                  -- Композиция функций
$                  -- Application функции
|>                 -- Pipeline (уникально)
<|                 -- Reverse pipeline (уникально)
>>=, >>            -- Monadic bind и then
<-                 -- Monadic bind в do-нотации
```

### 5.3. Специальные операторы
```glacier
//                 -- Целочисленное деление (Go)
**                 -- Возведение в степень
~                  -- Pattern negation (уникально)
@                  -- As-patterns (Haskell)
?                  -- Optional chaining (уникально)
```

## 6. Прочие лексемы

**Скобки и разделители:**
```glacier
()    -- Вызов функций, группировка
{}    -- Блоки кода, records (уникально)
[]    -- Списки, индексация
<>    -- Параметры типов (уникально)
,     -- Разделитель элементов
;     -- Разделитель выражений (опционально)
:     -- Annotation типа
->    -- Тип функции, лямбды
=>    -- Constraint в типах
```

## 7. Комментарии

**Многоуровневая система комментариев:**
```glacier
-- Однострочные комментарии (Haskell)
// Также однострочные (Go)

{-
 Многострочные комментарии (Haskell)
-}

/*
 И альтернативные многострочные (Go)
*/

/// Документирующие комментарии для функций
/// @param x - первое число
/// @param y - второе число
/// @return произведение чисел

#[
 Специальные аннотации (уникально)
 deprecated: "Use newFunction instead"
 experimental: true
]#
```

## 8. Уникальные особенности Glacier

### 8.1. Гибридные блоки
```glacier
-- Функциональный блок
funcBlock = do
    x <- getValue
    y <- process x
    pure (x + y)

-- Императивный блок
cmdBlock = cmd {
    print "Starting..."
    for i in 1..10 do
        when (i % 2 == 0) do
            print $ "Even: " ++ show i
    print "Done"
}
```

### 8.2. Pipe операторы
```glacier
-- Прямой pipe
result = [1,2,3,4,5]
    |> filter even
    |> map (* 3)
    |> foldl (+) 0

-- Обратный pipe
result = 0 <| foldl (+) <| map (* 3) <| filter even <| [1,2,3,4,5]
```

### 8.3. Pattern matching с Go-стилем
```glacier
-- Haskell-стиль с улучшениями
parseResult :: Result[T] -> String
parseResult result = case result of
    Ok value    -> "Success: " ++ show value
    Err msg     -> "Error: " ++ msg
    _           -> "Unknown"

-- Go-стиль с pattern matching
handleResponse resp = cmd {
    switch resp {
    case {status: 200, body: Ok data}:
        print "Success"
        process data

    case {status: 404, ...}:
        print "Not found"

    case {status: s, body: Err msg} if s >= 500:
        print $ "Server error: " ++ msg
    }
}
```

## 9. Правила форматирования

- **Отступы**: 2 пробела (как Go)
- **Максимальная длина строки**: 80 символов (как Haskell)
- **Именование**: Смешанный стиль как описано выше
- **Organization**: Модули как в Haskell, пакеты как в Go
```
