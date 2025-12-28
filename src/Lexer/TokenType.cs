namespace Lexer;
public enum TokenType
{
    /// <summary>
    /// Ключевое слово module
    /// </summary>
    Module,

    /// <summary>
    /// Ключевое слово import
    /// </summary>
    Import,

    /// <summary>
    /// Ключевое слово new
    /// </summary>
    New,

    /// <summary>
    /// Ключевое слово func
    /// </summary>
    Func,

    /// <summary>
    ///  Ключевое слово let.
    /// </summary>
    Let,

    /// <summary>
    /// Ключевое слов var
    /// </summary>
    Var,

    /// <summary>
    /// Ключевое слово enum
    /// </summary>
    Enum,

    /// <summary>
    /// Ключевое слово interface
    /// </summary>
    Interface,

    /// <summary>
    /// Ключевое слово if
    /// </summary>
    If,

    /// <summary>
    /// Ключевое слово then
    /// </summary>
    Then,

    /// <summary>
    /// Ключевое слово else
    /// </summary>
    Else,

    /// <summary>
    /// Ключевое слово for
    /// </summary>
    For,

    /// <summary>
    /// Ключевое слово while
    /// </summary>
    While,

    /// <summary>
    /// Ключевое слово do
    /// </summary>
    Do,

    /// <summary>
    /// Ключевое слово in
    /// </summary>
    In,

    /// <summary>
    /// Ключевое слово  return
    /// </summary>
    Return,

    /// <summary>
    /// Ключевое слово throw
    /// </summary>
    Throw,

    /// <summary>
    /// Ключевое слово try
    /// </summary>
    Try,

    /// <summary>
    /// Ключевое слово catch
    /// </summary>
    Catch,

    /// <summary>
    /// Ключевое слово type
    /// </summary>
    Type,

    /// <summary>
    /// Ключевое слово
    /// </summary>
    Continue,

    /// <summary>
    /// Ключевое слово
    /// </summary>
    Break,

    /// <summary>
    /// Ключевое слово where
    /// </summary>
    Where,

    /// <summary>
    /// Ключевое слово implements
    /// </summary>
    Implements,

    /// <summary>
    /// Конец файла
    /// </summary>
    EndOfFile,

    /// <summary>
    /// Оператор присваивания, равенства
    /// </summary>

    /// <summary>
    /// Идентификатор (имя символа).
    /// </summary>
    Identifier,

    /// <summary>
    /// Литерал числа.
    /// </summary>
    Integer,

    /// <summary>
    /// Литерал числа с плавющей точкой
    /// </summary>
    Float,

    /// <summary>
    /// Литерал строки в двойных кавычках
    /// </summary>
    StringLiteral,

    /// <summary>
    /// Левая фигурная скобка
    /// </summary>
    LBrace,

    /// <summary>
    /// Правая фигурная скобка
    /// </summary>
    RBrace,

    /// <summary>
    /// Левая круглая скобка
    /// </summary>
    LParenthesis,

    /// <summary>
    /// Правая круглая скобка
    /// </summary>
    RParenthesis,

    /// <summary>
    /// Левая квадратная скобка
    /// </summary>
    LSquareBracket,

    /// <summary>
    /// Правая квадратная скобка
    /// </summary>
    RSquareBracket,

    /// <summary>
    /// Оператор сравнения "меньше"
    /// </summary>
    LessThan,

    /// <summary>
    /// Открытие generic
    /// </summary>
    LessGeneric,

    /// <summary>
    /// Оператор сравнения "меньше или равно"
    /// </summary>
    LessThanOrEqual,

    /// <summary>
    /// Оператор сравнения "больше"
    /// </summary>
    GreaterThan,

    /// <summary>
    /// Оператор сравнеия "больше и равно"
    /// </summary>
    GreaterThanOrEqual,

    /// <summary>
    /// Закрытие generic
    /// </summary>
    GreaterGeneric,

    /// <summary>
    /// Оператор сложения +
    /// </summary>
    PlusSign,

    /// <summary>
    /// Оператор вычитания -
    /// </summary>
    MinusSign,

    /// <summary>
    /// Оператор умножения *
    /// </summary>
    MultiplySign,

    /// <summary>
    /// Оператор возведения в степень **
    /// </summary>
    Exponent,

    /// <summary>
    /// Оператор деления /
    /// </summary>
    DivideSign,

    /// <summary>
    /// Опертор целочисленного деления //
    /// </summary>
    IntegerDivide,

    /// <summary>
    /// Оператор деления с остатком %
    /// </summary>
    ModSign,

    /// <summary>
    /// Оператор присвоения
    /// </summary>
    Assign,

    /// <summary>
    /// Оператор равенства ==
    /// </summary>
    Equal,

    /// <summary>
    /// Логическое или ||
    /// </summary>
    LogicalOr,

    /// <summary>
    /// Побитове или |
    /// </summary>
    BitwiseOr,

    /// <summary>
    /// Логическое и  &&
    /// </summary>
    LogicalAnd,

    /// <summary>
    /// Побитовое и &
    /// </summary>
    BitwiseAnd,

    /// <summary>
    /// Логическое отрицание !
    /// </summary>
    LogicalNot,

    /// <summary>
    /// Логическое неравенство !=
    /// </summary>
    LogicalNotEqual,

    /// <summary>
    /// Побитовое не ~
    /// </summary>
    BitwiseNot,

    /// <summary>
    /// Побитовое XOR ~
    /// </summary>
    BitwiseXOR,

    /// <summary>
    /// Диапазон ..
    /// </summary>
    Range,

    /// <summary>
    /// Аннотация :
    /// </summary>
    Annotation,

    /// <summary>
    /// Точка с запятой ; (необязательный разделитель выражений)
    /// </summary>
    Semicolon,

    /// <summary>
    /// Запятая , (разделитель элемента)
    /// </summary>
    Comma,

    /// <summary>
    /// Доступ к полю/методу . (this.field)
    /// </summary>
    Access,

    /// <summary>
    /// Ключевое слово this
    /// </summary>
    This,

    /// <summary>
    /// Лексемма true
    /// </summary>
    True,

    /// <summary>
    /// Лексемма false
    /// </summary>
    False,

    /// <summary>
    /// нулевой литерал
    /// </summary>
    NullLiteral,

    /// <summary>
    /// Ключевое слово int
    /// </summary>
    IntegerType,

    /// <summary>
    /// Ключевое слово float
    /// </summary>
    FloatType,

    /// <summary>
    /// Ключевое слово string
    /// </summary>
    StringType,

    /// <summary>
    /// Клчюевое слово char
    /// </summary>
    CharType,

    /// <summary>
    /// Ключевое слово void
    /// </summary>
    VoidType,

    /// <summary>
    /// Ключевое слово bool
    /// </summary>
    BooleanType,

    /// <summary>
    /// Инкремент
    /// </summary>
    Increment,

    /// <summary>
    /// Дикремент
    /// </summary>
    Dicrement,

    /// <summary>
    /// Недопустимая лексема
    /// </summary>
    Error,
}
