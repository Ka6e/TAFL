using Antlr4.Runtime.Misc;
using Xunit;

namespace GlacierGrammar.UnitTests
{
    public class GlacierGrammarTest
    {
        // ----------------------------------------
        // Тесты для выражений
        // ----------------------------------------
        [Theory]
        [MemberData(nameof(GetValidExpressions))]
        public void Accepts_valid_expressions(string expression)
        {
            GlacierGrammar.ValidateExpression(expression);
        }

        [Theory]
        [MemberData(nameof(GetInvalidExpressions))]
        public void Rejects_invalid_expressions(string expression)
        {
            Assert.Throws<ParseCanceledException>(() => GlacierGrammar.ValidateExpression(expression));
        }

        public static TheoryData<string> GetValidExpressions()
        {
            return new TheoryData<string>
            {
                // Простые числа
                "42",
                "3.14",
                "0xFF",
                "0b1010",
                
                // Базовые арифметические операции
                "2 + 2",
                "10 - 5",
                "3 * 4",
                "15 / 3",
                "7 % 2",
                "10 // 3",
                "2 ** 3",
                
                // Приоритет операторов
                "2 + 3 * 4",
                "(2 + 3) * 4",
                "2 * 3 + 4 / 2",
                
                // Побитовые операции
                "5 & 3",
                "5 | 3",
                "5 ^ 3",
                "~1",
                
                // Операции сравнения
                "5 == 5",
                "3 != 4",
                "2 < 5",
                "5 <= 5",
                "10 > 2",
                "7 >= 7",
                
                // Сложные выражения
                "2 + 3 * (4 - 1)",
                "(a + b) * c - d / e",
                
                // Функции
                "ABS(-5)",
                "MIN(1, 2)",
                "MAX(3, 4, 5)",
                "POW(2, 3)",
                
                // Константы
                "PI",
                "EULER",
                
                // Комбинированные выражения с функциями
                "ABS(-5) + MIN(1, 2) * 3",
                "POW(2, 3) > MAX(1, 2)",
                
                // Унарные операции
                "-5",
                "+3.14"
            };
        }

        public static TheoryData<string> GetInvalidExpressions()
        {
            return new TheoryData<string>
            {
                "",       // Пустая строка
                "   ",    // Только пробелы
                "(2 + 3", // Незакрытая скобка
                "2 + 3)", // Лишняя закрывающая скобка
                "((2 + 3)",
                "(2 + 3))",
                "ABS(2 + 3", // Незакрытый вызов функции
                "2 @ 3",     // Неправильные операторы
                "5 $ 1",
                "3 * / 4",   // Неправильная последовательность операторов
                "+", "*", "()", // Отсутствие операндов
                "123ABC", "0xGG", "0b102", // Неправильные числа
                "2 3", "a b c",           // Неправильный порядок операций
                "MIN(1,,2)",              // Лишние запятые
                "2 +", "3 *", "ABS(", "(2 +" // Незавершенные выражения
            };
        }

        // ----------------------------------------
        // Тесты для программ (новые примеры аналитика)
        // ----------------------------------------
        [Theory]
        [MemberData(nameof(GetValidPrograms))]
        public void Accepts_valid_programs(string code)
        {
            GlacierGrammar.ValidateProgram(code);
        }

        public static TheoryData<string> GetValidPrograms()
        {
            return new TheoryData<string>
    {
        // ----------------------
        // 1. SumNumbers
        // ----------------------
        @"module Main
let a: int = readInt();
let b: int = readInt();
let sum: int = a + b;
print(sum);",

        // ----------------------
        // 2. GeometricMean
        // ----------------------
        @"module Main
let x: float = readInt();
let y: float = readInt();
let mean: float = (x * y) ** 0.5;
print(mean);",

        // ----------------------
        // 3. CircleSquare (исправлено Pi -> PI)
        // ----------------------
        @"module Main
let r: float = readInt();
let area: float = PI * r ** 2;
print(area);"
    };
        }

    }
}
