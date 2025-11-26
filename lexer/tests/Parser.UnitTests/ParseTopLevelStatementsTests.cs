using System.Globalization;

using Execution;

namespace Parser.UnitTests;
public class ParseTopLevelStatementsTests
{
    private const int Precision = 5;
    private readonly Context context;
    private readonly FakeEnvironment environment;

    public ParseTopLevelStatementsTests()
    {
        context = new Context();
        environment = new FakeEnvironment();
    }

    [Theory]
    [MemberData(nameof(GetTopLevelStatementData))]
    public void Can_parse_top_level_statements(string code, List<decimal> expected)
    {
        Parser parser = new Parser(context, environment, code);
        parser.ParseProgram();

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
                "x + 5;", [4, 9]
            },
            {
                "1 + 2; 2 * 5; 4.5;", [3, 10, 4.5m]
            },
            {
                "var x = 1;" +
                "var y = 2;" +
                "var z = 3;" +
                "x + y * z;", [1, 2, 3, 7]
            },
            {
                "var x = 10;" +
                "var y = x;" +
                "var z = y;" +
                "x + y + z;", [10, 10, 10, 30]
            },
            {
                "let pi: float = 3.14159;" +
                "4.0 * pi * 4.0;", [3.14159m, 50.26544m]
            },
            {
                "var a = 10;" +
                "var b = 2;" +
                "a = 5;" +
                "b = a + 1;", [10, 2, 5, 6]
            },
            {
                "var x;" +
                "let x:int;", [0, 0]
            },
            {
                "var y;" +
                "let x: int;", [0, 0]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetInvalidTopLevelStatementData))]
    public void Cannot_parse_top_level_statements(string code)
    {
        Parser parser = new Parser(context, environment, code);

        Assert.Throws<ArgumentException>(() => parser.ParseProgram());
    }

    public static TheoryData<string> GetInvalidTopLevelStatementData()
    {
        return new TheoryData<string>()
        {
            {
                "x + 1"
            },
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
        Parser parser = new Parser(context, environment, code);

        Assert.Throws<UnexpectedLexemeException>(() => parser.ParseProgram());
    }

    public static TheoryData<string> GetInvalidStatement()
    {
        return new TheoryData<string>()
        {
            { "var = 1; x + 2;" },
            { "let x = 1;" },
        };
    }
}
