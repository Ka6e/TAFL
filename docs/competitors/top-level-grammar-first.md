# Грамматика верхнего уровня языка Glacier

Glacier — объектно-ориентированный язык программирования с элементами функционального стиля.
Он сочетает принципы Go (простота, явность, блочная область видимости) и Haskell (чистота выражений, match-выражения), сохраняя строгую статическую типизацию и лаконичность синтаксиса.

---

## Пример программы

```glacier
module Main

import IO

enum DivisionResult {
    Success(value: int)
    Failure(message: string)
}

class Divider {
    let factor: int

    new(f: int) {
        this.factor = f
    }

    func divide(x: int, y: int): DivisionResult {
        if y == 0 then
            return DivisionResult.Failure("Cannot divide by zero")
        else
            return DivisionResult.Success(x / y)
    }
}

func main(): void {
    let divider = Divider.new(2)
    let result = divider.divide(10, 0)

    match result {
        case Success(v): print("Quotient: " + show(v))
        case Failure(msg): print("Error: " + msg)
    }
}
```

---

## Ключевые особенности языка

- Модульная структура (module, import)
- Классы, функции, перечисления (class, func, enum)
- Неизменяемые (let) и изменяемые (var) переменные
- Поддержка ООП и структурных типов
- Статическая типизация с аннотациями
- Встроенные функции ввода-вывода (print, readInt, readLine)
- Без неявных побочных эффектов — как в Go

---

## Семантические правила

- Переменная не может быть объявлена повторно в пределах одной области видимости
  (ошибка: `let x = 1; let x = 2;`)
- Использование необъявленной переменной вызывает ошибку времени компиляции
- Все функции с ненулевым возвращаемым типом должны возвращать значение
- `return` разрешён только внутри тела функции
- `let` создаёт неизменяемую, `var` — изменяемую переменную
- Объявления модулей и импортов допустимы только на верхнем уровне
- Классы определяют свои собственные области видимости и контекст `this`

---

## Виды инструкций

| Категория | Пример | Описание |
|-----------|--------|----------|
| Объявление переменной | `let x: int = 10;` | Неизменяемая переменная |
| Объявление изменяемой | `var sum = 0;` | Изменяемая переменная |
| Присваивание | `sum = sum + 1;` | Только для `var` |
| Вызов функции | `print("Hi");` | Ввод/вывод или вызов пользовательской функции |
| Условный блок | `if cond then ... else ...` | Ветвление |
| Цикл | `for i = 0, i < 10, i = i + 1 in { ... }` | Императивная форма |
| Возврат | `return result;` | Прерывает выполнение функции |
| Сопоставление | `match expr { case ... }` | Аналог switch с деструктуризацией |

---

## Грамматика в нотации EBNF

```ebnf
(* Корневая структура программы *)
program = module_decl, { import_decl }, { top_level_decl } ;

module_decl = "module", identifier ;
import_decl = "import", identifier ;

(* Верхнеуровневые объявления *)
top_level_decl = class_decl
                | enum_decl
                | function_decl
                | statement ;

(* Объявление класса *)
class_decl = "class", identifier, "{", { class_member }, "}" ;
class_member = variable_decl | function_decl ;

(* Объявление перечисления *)
enum_decl = "enum", identifier, "{", { enum_case }, "}" ;
enum_case = identifier, [ "(", parameter_list, ")" ] ;

(* Объявление функции *)
function_decl = "func", identifier, "(", [ parameter_list ], ")", [ ":", type_annotation ], block ;
parameter_list = parameter, { ",", parameter } ;
parameter = identifier, [ ":", type_annotation ] ;

(* Инструкции *)
statement = variable_decl
           | assignment
           | if_statement
           | for_statement
           | return_statement
           | match_statement
           | expression_statement ;

variable_decl = let_variable_decl | var_variable_decl;

let_variable_decl = "let", identifier, ":", type_annotation, [ "=", expression ], ";" ;

var_variable_decl = "var, identifier, [ "=", expression ], ";" ;

assignment = identifier, "=", expression, ";" ;

if_statement = "if", expression, "then", block, [ "else", block ] ;
for_statement = "for", assignment, expression, ",", expression, "in", block ;
return_statement = "return", [ expression ], ";" ;

match_statement = "match", expression, "{", { match_case }, "}" ;
match_case = "case", identifier, [ "(", identifier, ")" ], ":", statement ;

expression_statement = expression, ";" ;

block = "{", { statement }, "}" ;

(* Типы и базовые элементы *)
type_annotation = "int" | "float" | identifier ;
expression = (* см. docs/specification/expressions-grammar.md *) ;
identifier = letter, { letter | digit | "_" } ;
```

---

## Примеры инструкций

```glacier
let radius: float = 5.0
var area = Pi * radius ** 2

if area > 10.0 then
    print("Big circle")
else
    print("Small circle")

for i = 0, i < 5, i = i + 1 in {
    print("Step " + show(i))
}

match result {
    case Success(v): print(v)
    case Failure(msg): print("Error: " + msg)
}
```

---

## Комментарии

Поддерживаются оба вида комментариев:

```glacier
// Однострочный комментарий

/* Многострочный
   комментарий */
```

---

## Замечания к грамматике

- Грамматика не содержит левой рекурсии, подходит для рекурсивного спуска
- Все выражения определены в отдельном файле `docs/specification/expressions-grammar.md`
- Поддерживается последовательное исполнение инструкций
- Разделители инструкций — символ `;`
- Ввод-вывод реализуется как вызовы встроенных функций (`print`, `readInt`, `readLine`)
- Встроенные функции и классы можно использовать без предварительного импорта `IO`

