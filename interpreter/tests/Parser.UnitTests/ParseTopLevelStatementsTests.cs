using Interpreter;

namespace Parser.UnitTests.TopLevelStatmentsTests;
public class ParseTopLevelStatementsTests
{
    private const int Precision = 5;
    private readonly GlacierInterpreter interpreter;
    private readonly FakeEnvironment environment;

    public ParseTopLevelStatementsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetTopLevelStatementData))]
    public void Can_parse_top_level_statements(string code, List<decimal> expected)
    {
        string codeExp = "module main\n" + code;

        interpreter.Execute(codeExp);

        IReadOnlyList<decimal> actual = environment.Results;
        for (int i = 0, iMax = Math.Min(expected.Count, actual.Count); i < iMax; i++)
        {
            Assert.Equal(expected[i], actual[i], Precision);
        }

        if (expected.Count != actual.Count)
        {
            Assert.Fail(
                $"Actual results count does not match expected. Expected: {expected.Count}, Actual: {actual.Count}."
            );
        }
    }

    public static TheoryData<string, List<decimal>> GetTopLevelStatementData()
    {
        return new TheoryData<string, List<decimal>>()
        {
            {
                "var x = 2 + 2;" +
                "x + 5;", [9]
            },
            {
                "1 + 2; 2 * 5;", [3, 10]
            },
            {
                "var x = 1;" +
                "var y = 2;" +
                "var z = 3;" +
                "x + y * z;", [7]
            },
            {
                "var x = 10;" +
                "var y = x;" +
                "var z = y;" +
                "x + y + z;", [30]
            },
            {
                "var a = 10;" +
                "var b = 2;" +
                "a = 5;" +
                "b = a + 1;", [5, 6]
            },
            {
                "let x:int;", [0]
            },
            {
                "1.0;", [1.0m]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetInvalidTopLevelStatementData))]
    public void Cannot_parse_top_level_statements(string code)
    {
        string codeExp = "module Main\n" + code;

        Assert.Throws<ArgumentException>(() => interpreter.Execute(codeExp));
    }

    public static TheoryData<string> GetInvalidTopLevelStatementData()
    {
        return new TheoryData<string>()
        {
            {
                "let x: int = 1; " +
                "let x: int = 10;"
            },
            {
                "var x = 10;" +
                "var x = 50;"
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetInvalidStatement))]
    public void Cannot_parse_top_level_statement(string code)
    {
        string codeExp = "module Main\n" + code;

        Assert.Throws<UnexpectedLexemeException>(() => interpreter.Execute(codeExp));
    }

    public static TheoryData<string> GetInvalidStatement()
    {
        return new TheoryData<string>()
        {
            { "var = 1; x + 2;" },
            { "let x = 1;" },
            { "x + 1" },
        };
    }
}
