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
        string path = CreateTempFile("func sumArray(arr: [int]): int {\r\n    let sum = 0\r\n    for i in 0..(arr.length - 1) {\r\n        sum = sum + arr[i]\r\n    }\r\n    return sum\r\n}");

        string result = LexicalStats.CollectFromFile(path);

        string expected =
            $"keywords: 7{Environment.NewLine}" +
            $"identifier: 11{Environment.NewLine}" +
            $"number literals: 3{Environment.NewLine}" +
            $"string literals: 0{Environment.NewLine}" +
            $"operators: 6{Environment.NewLine}" +
            "other lexemes: 14";
        Assert.Equal(expected, result);
    }

    private string CreateTempFile(string content)
    {
        string path = Path.GetTempFileName();
        File.WriteAllText(path, content, Encoding.UTF8);
        return path;
    }
}
