# Грамматика верхнего уровня языка Glacier

Этот документ описывает грамматику верхнего уровня (top-level) языка Glacier:
модули, импорты, объявления типов и подпрограмм, а также инструкции (statements).
Glacier — объектно-ориентированный императивный язык с элементами функционального стиля.
Ветки и циклы в языке — инструкции.

---

## Структура программы

Программа состоит из объявления модуля, нуля или более импортов и списка верхнеуровневых объявлений.

Пример:

```glacier
module Main

import IO

class X { ... }

func main(): void {
    ...
}
```

---

## Ключевые правила и ограничения (семантика)

*   `module` и `import` — только на верхнем уровне.
*   `return` разрешён только внутри тела функции.
*   `break` и `continue` разрешены только внутри тела цикла (for / while / do-while).
*   Повторное объявление переменной в одной области видимости — ошибка компиляции.
*   Все функции с ненулевым возвращаемым типом должны возвращать значение по всем путям исполнения.
*   Блочная область видимости: `{ ... }` создаёт новую область.
*   `this` доступен внутри методов класса.

---

## Решение проблемы «висячего else» (dangling else)

В Glacier применяется обычное, простое правило: `else` ассоциируется с ближайшим не закрытым `if`.
Чтобы убрать неоднозначности в крупных конструкциях, в спецификации рекомендуется:
*   Для многострочных ветвей использовать блоки `{ ... }`.
*   Использовать явные `then` / `else` и скобки — синтаксис языка уже требует `then` после условия:
    `if <expr> then <block> [ else <block> ]`.

Пример (без неоднозначности):

```glacier
if cond1 then {
    if cond2 then { ... } else { ... }  // else связывается с ближайшим if
}
```

При желании программист может всегда добавить `{}` вокруг ветвей, чтобы сделать намерение очевидным.

---

## Верхнеуровневые объявления и инструкции — описание

### Модули и импорты

```
module_decl = "module", identifier ;
import_decl = "import", identifier ;
```

### Верхнеуровневые объявления

Поддерживаются объявления классов, перечислений и функций. Также возможны инструкции на верхнем уровне (в виде statement), если язык собирается исполнять код при загрузке модуля.

```
top_level_decl = class_decl
               | enum_decl
               | function_decl
               | statement ;
```

### Классы и члены

```
class_decl = "class", identifier, "{", { class_member }, "}" ;
class_member = variable_decl | function_decl ;
```

### Перечисления (enum)

```
enum_decl = "enum", identifier, "{", { enum_case }, "}" ;
enum_case = identifier, [ "(", parameter_list, ")" ] ;
```

### Функции

Функция объявляется как `func <name>([params]): [type] block`. Тело функции — блок `{ ... }`. `return` можно использовать внутри этого блока.

```
function_decl = "func", identifier, "(", [ parameter_list ], ")", [ ":", type_annotation ], block ;
parameter_list = parameter, { ",", parameter } ;
parameter = identifier, [ ":", type_annotation ] ;
```

Уточнение: функции без параметров — разрешены (пустой `()`).

---

## Инструкции (statements)

Список поддерживаемых инструкций:
*   `variable_decl` — объявление переменной (`let` / `var`)
*   `assignment` — присваивание
*   `if_statement` — ветвление
*   `for_statement` — императивный цикл for (трёхчастный)
*   `while_statement` — цикл while
*   `do_while_statement` — цикл do ... while
*   `break_statement` — прерывание цикла
*   `continue_statement` — переход к следующей итерации
*   `return_statement` — возврат из функции
*   `match_statement` — сопоставление (match)
*   `expression_statement` — выражение как инструкция (вызов функции и т.п.)

```
statement = variable_decl
          | assignment
          | if_statement
          | for_statement
          | while_statement
          | do_while_statement
          | break_statement
          | continue_statement
          | return_statement
          | match_statement
          | expression_statement ;
```

### Объявление переменной

```
variable_decl = ("let" | "var"), identifier, [ ":", type_annotation ], [ "=", expression ], ";" ;
```

`let` — неизменяемая, `var` — изменяемая.

### Присваивание

```
assignment = identifier, "=", expression, ";" ;
```

### if — ветвление (императивный)

`if` требует ключевого слова `then`. Тело ветки — `block`. `else` опционален.

```
if_statement = "if", expression, "then", block, [ "else", block ] ;
```

Пример:

```glacier
if x > 0 then {
    print("positive");
} else {
    print("non-positive");
}
```

### for — императивный цикл (трёхчастный)

Форма: `for <init>, <condition>, <post> in <block>`
`init` и `post` — обычно assignment (или пустые при необходимости). `condition` — выражение булеан/числовое. Тело — `block`.

```
for_statement = "for", assignment_or_empty, expression, ",", assignment_or_empty, "in", block ;
assignment_or_empty = assignment | /* empty assignment placeholder */ ;
```

Пример:

```glacier
for i = 0, i < 10, i = i + 1 in {
    ...
}
```

### while

Стандартный цикл проверки перед первой итерацией:

```
while_statement = "while", "(", expression, ")", block ;
```

Пример:

```glacier
while (x < 10) {
    x = x + 1;
}
```

### do-while

Проверка после первой итерации:

```
do_while_statement = "do", block, "while", "(", expression, ")", ";" ;
```

Пример:

```glacier
do {
    x = x - 1;
} while (x > 0);
```

### break / continue

Простые инструкции внутри циклов:

```
break_statement = "break", ";" ;
continue_statement = "continue", ";" ;
```

Семантика: `break` выходит из текущего (вложенного) цикла; `continue` пропускает оставшуюся часть тела и начинает следующую итерацию текущего цикла.

### return

```
return_statement = "return", [ expression ], ";" ;
```

`return` разрешён только внутри тела функции. Для функций с возвращаемым типом `void` `return` без выражения допустим; для функций с другим типом — должен возвращаться корректный тип.

### match (сопоставление)

```
match_statement = "match", expression, "{", { match_case }, "}" ;
match_case = "case", pattern, ":", block ;
pattern = identifier | enum_pattern | "_" ;
enum_pattern = identifier, "(", [ identifier_list ], ")" ;
identifier_list = identifier, { ",", identifier } ;
```

---

## Блоки и инструкции

```
block = "{", { statement }, "}" ;
expression_statement = expression, ";" ;
```

---

## Полная EBNF-грамматика верхнего уровня

```
(* Корневая структура программы *)
program = module_decl, { import_decl }, { top_level_decl } ;

module_decl = "module", identifier ;
import_decl = "import", identifier ;

top_level_decl = function_decl
               | statement ;

function_decl = "func", identifier, "(", [ parameter_list ], ")", [ ":", type_annotation ], block ;
parameter_list = parameter, { ",", parameter } ;
parameter = identifier, [ ":", type_annotation ] ;

statement = variable_decl
          | if_statement
          | for_statement
          | while_statement
          | do_while_statement
          | break_statement
          | continue_statement
          | return_statement
          | expression_statement ;

variable_decl = "let", identifier, ":", type_annotation, [ "=", expression ], ";"
              | "var", identifier, "=", expression, ";" ;

if_statement = "if", expression, "then", block, [ "else", block ] ;

for_statement = "for", [ assignment_expression ] ",", [ expression ] ",", [ assignment_expression ] , "in", block ;

while_statement = "while", "(", expression, ")", block ;
do_while_statement = "do", block, "while", "(", expression, ")", ";" ;

break_statement = "break", ";" ;
continue_statement = "continue", ";" ;

return_statement = "return", [ expression ], ";" ;

expression_statement = expression, ";" ;
block = "{", { statement }, "}" ;

type_annotation = int | string | bool | float ;
expression = (* смотрите expressions-grammar.md *) ;

identifier = ( letter | "_" ), { letter | digit | "_" } ;
```
