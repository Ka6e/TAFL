# Glacier — Руководство (ООП, 50% Go + 50% Haskell)

**Цель:** ясный, читаемый язык программирования в ООП-стиле, сочетающий простоту и прагматичность Go с выразительностью и строгой типовой дисциплиной Haskell. Язык остаётся императивно-ООП: классы/объекты, интерфейсы, методы — но принимает лучшие идеи из Haskell: выражения, явные типы, `enum`-типизацию, неизменяемость по умолчанию и безопасную работу с ошибками.

---

## 1. Пример кода — полный, в стиле OOP, сочетающий идеи

```glacier
module Main

import IO, Collections

// Enum (Haskell-подобный tagged union)
enum Result[T, E] {
  Ok(value: T)
  Err(error: E)
}

// Класс как в OOP, поля по умолчанию immutable (let)
class Calculator {
  let factor: Int

  // Конструктор
  new(f: Int) {
    this.factor = f
  }

  // Метод — camelCase
  func multiply(x: Int): Int {
    return x * this.factor
  }

  // Метод возвращает Result — стиль Haskell, но вызывается по-ООП
  func safeDivide(x: Int, y: Int): Result[Int, String] {
    if y == 0 then
      return Result.Err("division by zero")
    else
      return Result.Ok(x / y)
  }
}

// Интерфейс (Go-подобный), с generic constraint (Haskell spirit)
interface Showable {
  show(): String
}

class Point implements Showable {
  let x: Int
  let y: Int
  new(x: Int, y: Int) {
    this.x = x
    this.y = y
  }
  func show(): String {
    return "(" ++ show(this.x) ++ ", " ++ show(this.y) ++ ")"
  }
}

func main(): Void {
  let calc = Calculator.new(3)

  let r = calc.safeDivide(10, 0)
  match r {
    case Ok(v): print("Result: " ++ show(v))
    case Err(e): print("Error: " ++ e)
  }

  let p = Point.new(1, 2)
  print(p.show())

  // mutability example
  var counter: Int = 0
  for i in 1..5 {
    counter = counter + 1
  }
  print("Counter: " ++ show(counter))
}
```

---

## 2. Ключевые слова (сбалансированные — половина из Go, половина из Haskell)

| Ключевое слово        | Предназначение                  | Вдохновение              |
| --------------------- | ------------------------------- | ------------------------ |
| `module`              | Объявление модуля               | Haskell                  |
| `import`              | Импорт модулей/пакетов          | Go                       |
| `class`               | Объявление класса/типа          | OOP                      |
| `new`                 | Конструктор / создание объекта  | Go/Haskell               |
| `func`                | Объявление функции/метода       | Go                       |
| `let`                 | Неизменяемая привязка           | Haskell                  |
| `var`                 | Изменяемая переменная           | Go                       |
| `enum`                | Tagged unions / ADT-like        | Haskell                  |
| `interface`           | Описание интерфейса             | Go                       |
| `if`/`then`/`else`    | Условное выражение              | Haskell-style expression |
| `match`               | Паттерн-матчинг/распаковка enum | Haskell                  |
| `for`                 | Циклы (range/for)               | Go                       |
| `return`              | Возврат значения                | Both                     |
| `throw`/`try`/`catch` | Исключения (опционально)        | OOP                      |
| `type`                | Синоним/alias типов             | Haskell/Go               |
| `where`               | Ограничения/квалификаторы типов | Haskell                  |
| `implements`          | Явная реализация интерфейса     | Java/Go-ish              |

---

## 3. Идентификаторы и соглашения об именовании

- Функции и методы: `camelCase` (как в Go). Примеры: `calculateTotal`, `getUserById`.
- Классы, типы, модули: `PascalCase`. Примеры: `User`, `Result`, `TreeMap`.
- Константы: `SCREAMING_SNAKE_CASE`.
- Параметры типов (generics): `T`, `K`, `V`.

---

## 4. Литералы

### 4.1 Числа

- `42`, `0x2A`, `0b1010`, `1_000_000` — целые
- `3.14`, `6.022e23` — плавающие

### 4.2 Строки

- Простые: `"Hello"`
- Интерполяция: `"Value: ${x}"` (хаскелльский дух — выражения в строках)
- Многострочные: `"""..."""`
- Raw-string: `` `C:\path\file` ``

### 4.3 Специальные

- Булевы: `true`, `false`
- Пусто/отсутствует: `null` или `None` (Option-стиль)

---

## 5. Операторы (микс Go и Haskell)

**Арифметика и сравнения:** `+ - * / %`, `== != < > <= >=`

**Логика:** `&& || !`

**Функционально-выраженные операторы:**

- `++` — конкатенация строк/списков (Haskell)
- `->` — тип функции (Haskell influence)

**Специальные:**

- `..` — диапазоны (например `1..10`)

---

## 6. Лексемы и скобки

- `()` — вызов функций и группирование
- `{}` — блоки, объявления классов/модулей
- `[]` — списки/массивы
- `<>` — параметры типов/дженерики
- `,` — разделитель
- `;` — опциональный разделитель выражений
- `:` — аннотация типа

---

## 7. Комментарии и документация

```glacier
// однострочный
/* много
строчный */
/// Док-комментарий для генерации API
```

---

## 8. Типовая система — 50/50 баланс

- **Явные типы** в духе Haskell: функция обычно подписывается `func f(x: Int): Int`.
- **Сильная статическая типизация** — ошибки типов ловятся на этапе компиляции.
- **Тип вывода** поддерживается, но не агрессивен: язык предпочитает явность.
- **Tagged unions / enum** — ADT-подобные конструкции (Haskell) для безопасных вариантов.
- **Generics** (Go-like, но с ограничениями/constraints из Haskell):

```glacier
func sum[T: Numeric](arr: List[T]): T { ... }
```

- **Option/Result** — стандартная практика возвращать `Result[T, E]` или `Option[T]`.

---

## 9. ООП: классы, объекты, интерфейсы (ядро)

- `class` — объявление типа с полями и методами.
- Поля по умолчанию `let` (immutable); `var` — если нужно мутировать.
- `new(...)` — конструктор, создание через `Type.new(...)`.
- `interface` — декларация набора методов (duck-typing по контракту, явно через `implements` или неявно как в Go).
- Комбинируем композицию и интерфейсы; наследование разрешено, но не поощряется.

Пример:

```glacier
interface Storage {
  save(key: String, value: String): Void
  load(key: String): Option[String]
}

class MemoryStorage implements Storage {
  var map: Map[String,String] = Map.new()
  func save(k, v) { this.map[k] = v }
  func load(k) { return this.map.contains(k) ? Some(this.map[k]) : None }
}
```

---

## 10. Pattern matching и `match` (Haskell-приближённо)

`match` — безопасный и выразительный способ работы с `enum`/`Result`/`Option`:

```glacier
let r = calc.safeDivide(10, 2)
match r {
  case Ok(v): print("ok: " ++ show(v))
  case Err(e): print("err: " ++ e)
}
```

Поддерживает `if`-guards и `_` как wildcard.

---

## 11. Выражения vs инструкции

- `if` — это выражение (как в Haskell): `let sign = if x > 0 then "+" else "-"`
- `match` возвращает значение.
- Это позволяет писать компактнее и ближе к функциональной мысли без монад.

---

## 12. Ошибки и обработка (два подхода)

1. **Явный Result/Option** (рекомендовано): безопасно и функционально:

```glacier
func parseInt(s: String): Result[Int, ParseError]
```

2. **Исключения** (`throw`/`try`/`catch`) — для случаев, где удобнее классический OOP-подход.

---

## 13. Конкурентность (Go-стиль, но безопаснее)

- `go` — запустить процедуру/метод в лёгком потоке.
- `Channel[T]` — типизированные каналы для общения между потоками.
- Также поддержка `async/await`-подобной синтаксической сахарщины для удобства.

```glacier
ch := Channel[Int].new()
go worker(ch)
ch.send(5)
let v = ch.receive()
```

---

## 14. Стандартные контейнеры и коллекции

- `List[T]`, `Array[T]`, `Map[K,V]`, `Set[T]`
- Функции коллекций: `map`, `filter`, `fold` — как вспомогательные методы на коллекциях (императивно-доступные)

---

## 15. Форматирование и стиль

- Отступы: **2 пробела**
- Максимальная длина строки: **80 символов**
- Именование: см. выше
- Файловая организация: модуль == файл (как в Haskell/Go)

---

## 16. Примеры типовых шаблонов

**Factory + Interface**

```glacier
interface Notifier { notify(msg: String): Void }
class EmailNotifier implements Notifier { ... }
class SMSNotifier implements Notifier { ... }

func NewNotifier(kind: String): Notifier {
  switch kind {
    case "email": return EmailNotifier.new()
    case "sms": return SMSNotifier.new()
    default: throw "unknown notifier"
  }
}
```

**Safe parsing**

```glacier
func readIntSafe(s: String): Result[Int, String] {
  try {
    let v = Integer.parse(s)
    return Result.Ok(v)
  } catch (e) {
    return Result.Err("not an int")
  }
}
```

---

## 17. Cheatsheet (коротко)

- Создание класса: `class Name { let a: Int; new(a:Int) { this.a = a } }`
- Вызов конструктора: `let o = Name.new(5)`
- Интерфейс: `interface I { m(): Int }`
- Enum: `enum Maybe[T] { Some(v:T) None }`
- Match: `match e { case A(x): ... case B: ... }`
- Generics: `func id[T](x:T): T { return x }`
- Immutability: `let x = 5` vs `var x = 5`

---

## 18. Резюме — дизайн-решения

- Ядро языка — ООП: классы, объекты, интерфейсы.
- Из Go взяли простоту синтаксиса, модель пакетов/модулей, и concurrency primitives.
- Из Haskell — явные типы, tagged unions (enum), `match`, выражения и философия неизменяемости.
- Баланс 50/50 достигается тем, что все современные безопасные конструкции Haskell доступны и рекомендуются, но использовать их можно императивно и в привычном ООП-стиле.

---

Если хочешь, могу:

- Прописать BNF/формальную грамматику для этого языка;
- Переписать твой первоначальный функциональный пример полностью в этом ООП-стиле;
- Сгенерировать одностраничный cheat-sheet в формате PDF/Markdown.

Напиши, что делать дальше.

