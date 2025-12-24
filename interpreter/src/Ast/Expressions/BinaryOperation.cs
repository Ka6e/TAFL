namespace Ast.Expressions;
public enum BinaryOperation
{
    /// <summary>
    /// Сложение чисел
    /// </summary>
    Add,

    /// <summary>
    /// Вычитание чисел
    /// </summary>
    Subtract,

    /// <summary>
    /// Умножение
    /// </summary>
    Multiply,

    /// <summary>
    /// Деление
    /// </summary>
    Divide,

    /// <summary>
    /// Логическое "ИЛИ"
    /// </summary>
    Or,

    /// <summary>
    /// Логическое  "И"
    /// </summary>
    And,

    /// <summary>
    /// Оператор сравнения "равно"
    /// </summary>
    Equal,

    /// <summary>
    /// Оператор сравнения "не равно"
    /// </summary>
    NotEqual,

    /// <summary>
    /// Оператор сравнения "больше"
    /// </summary>
    GreaterThan,

    /// <summary>
    /// Оператор сравнения "больше или равно"
    /// </summary>
    GreaterThanOrEqual,

    /// <summary>
    /// Оператор сравнения "меньше"
    /// </summary>
    LessThan,

    /// <summary>
    /// Оператор сравнения "меньше или равно"
    /// </summary>
    LessThanOrEqual,

    /// <summary>
    /// Остаток от деления
    /// </summary>
    Module,

    /// <summary>
    /// Возведение в степень
    /// </summary>
    Exponent,
}
