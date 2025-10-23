using System.Text;

namespace Lexer.UnitTests.LexicalStats;
public class LexicalStatsTests
{
    [Fact]
    public void CollectFromFile_WithEmptyFile_ReturnsZeroStats()
    {
        string path = CreateTempFile("");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 0{Environment.NewLine}" +
            $"identifier: 0{Environment.NewLine}" +
            $"number literals: 0{Environment.NewLine}" +
            $"string literals: 0{Environment.NewLine}" +
            $"operators: 0{Environment.NewLine}" +
            "other lexemes: 0";
        Assert.Equal(expected, result);
    }

    [Fact]
    public void CollectFromFile_WithSimpleProgramm_ReturnsCorrectStats()
    {
        string path = CreateTempFile("module Main\r\n\r\nimport IO\r\n\r\nfunc main(): void {\r\n    let x = 10\r\n    let y = 20\r\n    let sum = x + y\r\n    print(\"Sum is: \" + show(sum))\r\n}");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 7{Environment.NewLine}" +
            $"identifier: 11{Environment.NewLine}" +
            $"number literals: 2{Environment.NewLine}" +
            $"string literals: 1{Environment.NewLine}" +
            $"operators: 6{Environment.NewLine}" +
            "other lexemes: 8";
        Assert.Equal(expected, result);
    }

    [Fact]
    public void CollectFromFile_WithConditionalProgramm_ReturnsCorrectStats()
    {
        string path = CreateTempFile("module Main\r\n\r\nimport IO\r\n\r\nfunc checkNumber(n: int): string {\r\n    if n > 0 then\r\n        return \"positive\"\r\n    else if n < 0 then\r\n        return \"negative\"\r\n    else\r\n        return \"zero\"\r\n}\r\n\r\nfunc main(): void {\r\n    let result = checkNumber(42)\r\n    print(\"The number is: \" + result)\r\n}");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 17{Environment.NewLine}" +
            $"identifier: 11{Environment.NewLine}" +
            $"number literals: 3{Environment.NewLine}" +
            $"string literals: 4{Environment.NewLine}" +
            $"operators: 7{Environment.NewLine}" +
            "other lexemes: 12";
        Assert.Equal(expected, result);
    }

    [Fact]
    public void CollectFromFile_WithLoop_ReturnsCorrectStats()
    {
        string path = CreateTempFile("func sumArray(arr: [int]): int {\r\n    let sum = 0\r\n    for i in 0 .. (arr.length - 1) {\r\n        sum = sum + arr[i]\r\n    }\r\n    return sum\r\n}");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 7{Environment.NewLine}" +
            $"identifier: 11{Environment.NewLine}" +
            $"number literals: 3{Environment.NewLine}" +
            $"string literals: 0{Environment.NewLine}" +
            $"operators: 8{Environment.NewLine}" +
            "other lexemes: 12";
        Assert.Equal(expected, result);
    }

    [Fact]
    public void CollectFromFile_WithDifficulProgramm_ReturnsCorrectStats()
    {
        string path = CreateTempFile("module Main\r\n\r\nimport IO\r\n\r\nenum DivisionResult {\r\n    Success(value: int)\r\n    Failure(message: string)\r\n}\r\n\r\nclass Divider {\r\n    let factor: int\r\n\r\n    new(f: int) {\r\n        this.factor = f\r\n    }\r\n\r\n    func divide(x: int, y: int): DivisionResult {\r\n        if y == 0 then\r\n            return DivisionResult.Failure(\"Cannot divide by zero\")\r\n        else\r\n            return DivisionResult.Success(x / y)\r\n    }\r\n}\r\n\r\nfunc main(): void {\r\n    let divider = Divider.new(2)\r\n    let result = divider.divide(10, 0)\r\n\r\n    match result {\r\n        case Success(v): print(\"Quotient: \" + show(v))\r\n        case Failure(msg): print(\"Error: \" + msg)\r\n    }\r\n}");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 27{Environment.NewLine}" +
            $"identifier: 39{Environment.NewLine}" +
            $"number literals: 4{Environment.NewLine}" +
            $"string literals: 3{Environment.NewLine}" +
            $"operators: 22{Environment.NewLine}" +
            "other lexemes: 42";
        Assert.Equal(expected, result);
    }

    private string CreateTempFile(string content)
    {
        string path = Path.GetTempFileName();
        File.WriteAllText(path, content, Encoding.UTF8);
        return path;
    }
}
