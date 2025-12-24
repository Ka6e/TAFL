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
                "",      
                "   ",   
                "(2 + 3",
                "2 + 3)", 
                "((2 + 3)",
                "(2 + 3))",
                "ABS(2 + 3", 
                "2 @ 3",     
                "5 $ 1",
                "3 * / 4",   
                "+", "*", "()", 
                "123ABC", "0xGG", "0b102", 
                "2 3", "a b c",          
                "MIN(1,,2)",         
                "2 +", "3 *", "ABS(", "(2 +" 
            };
        }

        // Тесты для программ
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
                // SumNumbers

                @"module Main
                let a: int = readInt();
                let b: int = readInt();
                let sum: int = a + b;
                print(sum);",

                //GeometricMean

                @"module Main
                let x: float = readInt();
                let y: float = readInt();
                let mean: float = (x * y) ** 0.5;
                print(mean);",

                //CircleSquare

                @"module Main
                let r: float = readInt();
                let area: float = PI * r ** 2;
                print(area);",


                // isPrime 
                @"module Main
                func main() {
                    let n: int = readInt();
                    if n <= 1 then {
                        print(0);
                        return;
                    }
                    let i: int = 2;
                    let isPrime: int = 1;
                    while (i * i <= n) {
                        if n % i == 0 then {
                            isPrime = 0;
                            break;
                        }
                        i = i + 1;
                    }
                    print(isPrime);
                }",

                // Pow 
                @"module Main
                func main() {
                    let A: int = readInt();
                    let B: int = readInt();
                    let result: int = 1;
                    let i: int = 0;
                    for i = 0, i < B, i = i + 1 in {
                        result = result * A;
                    }
                    print(result);
                }",

                // SumDigits
                @"module Main
                func main() {
                    let n: int = readInt();
                    let sum: int = 0;
                    while (n > 0) {
                        let digit: int = n % 10;
                        sum = sum + digit;
                        n = n / 10;
                    }
                    print(sum);
                }",
                @
            };
        }
    }
}
