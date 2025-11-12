using Antlr4.Runtime.Misc;
using Xunit;

namespace GlacierGrammar.UnitTests
{
    public class GlacierGrammarTest
    {
        [Theory]
        [MemberData(nameof(GetValidExpressions))]
        public void Accepts_valid_expressions(string expression)
        {   
            // Act & Assert - не должно бросать исключение
            GlacierGrammar.ValidateExpression(expression);
        }

        [Theory]
        [MemberData(nameof(GetInvalidExpressions))]
        public void Rejects_invalid_expressions(string expression)
        {
            // Act & Assert - должно бросить исключение
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
                // Пустая строка
                "",
                
                // Только пробелы
                "   ",
                
                // Незакрытая скобка
                "(2 + 3",
                
                // Лишняя закрывающая скобка
                "2 + 3)",
                
                // Несбалансированные скобки
                "((2 + 3)",
                "(2 + 3))",
                
                // Незакрытый вызов функции
                "ABS(2 + 3",
                
                // Неправильные операторы
                "2 @ 3",
                "5 $ 1",
                
                // Неправильная последовательность операторов
                "3 * / 4",
                
                // Отсутствие операндов
                "+",
                "*",
                "()",
                
                // Неправильные числа
                "123ABC",
                "0xGG",
                "0b102",
                
                // Неправильный порядок операций
                "2 3",
                "a b c",
                
                // Лишние запятые
                "MIN(1,,2)",
                
                // Незавершенные выражения
                "2 +",
                "3 *",
                "ABS(",
                "(2 +"
            };
        }
    }
}