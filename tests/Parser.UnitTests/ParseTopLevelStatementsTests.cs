using Interpreter;

using Runtime;

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
    public void Can_parse_top_level_statements(string code, List<Value> expected)
    {
        string codeExp = "module main\n" + code;

        interpreter.Execute(codeExp);

        IReadOnlyList<Value> actual = environment.Results;
        for (int i = 0, iMax = Math.Min(expected.Count, actual.Count); i < iMax; i++)
        {
            Assert.Equal(expected[i].AsInt(), actual[i].AsInt());
        }

        if (expected.Count != actual.Count)
        {
            Assert.Fail(
                $"Actual results count does not match expected. Expected: {expected.Count}, Actual: {actual.Count}."
            );
        }
    }

    public static TheoryData<string, List<Value>> GetTopLevelStatementData()
    {
        return new TheoryData<string, List<Value>>()
        {
            {
                "var x = 2 + 2;" +
                "print(x + 5);", [new Value(9)]
            },
            {
                "print(1 + 2); print(2 * 5);", [new Value(3), new Value(10)]
            },
            {
                "var x = 1;" +
                "var y = 2;" +
                "var z = 3;" +
                "print(x + y * z);", [new Value(7)]
            },
            {
                "var x = 10;" +
                "var y = x;" +
                "var z = y;" +
                "x + y + z;" +
                "print(x+y+z);", [new Value(30)]
            },
            {
                "var a = 10;" +
                "var b = 2;" +
                "a = 5;" +
                "b = a + 1;" +
                "print(a);" +
                "print(b);", [new Value(5), new Value(6)]
            },
            {
                "let x:int;" +
                "print(x);", [new Value(0)]
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
