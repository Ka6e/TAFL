using Execution;

using Interpreter;

namespace Parser.UnitTests;
public class BuiltinFunctionsTests
{
    private const int Precision = 5;
    private readonly GlacierInterpreter interpreter;
    private readonly FakeEnvironment environment;

    public BuiltinFunctionsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetBuiltinFunctions))]
    public void Handle_Built_In_Functions(string expression, decimal expected)
    {
        string code = $"module Main\n print({expression});";

        interpreter.Execute(code);

        decimal result = Assert.Single(environment.Results);

        Assert.Equal(expected, result, Precision);
    }

    public static TheoryData<string, decimal> GetBuiltinFunctions()
    {
        return new TheoryData<string, decimal>()
        {
            { "abs(-10)", 10m },
            { "abs(-7.5)", 7.5m },
            { "min(10, 4, 1, 56)", 1m },
            { "min(-1, -50, 0)", -50m },
            { "max(1, 2, 3, 4, 5)", 5m },
            { "max(-10, -8, -1)", -1m },
            { "pow(-1, 2)", 1m },
            { "pow(1, 2)", 1m },
            { "pow(2, 3)", 8m },
            { "pow(-2, 3)", -8m },
            { "round(3.2)", 3m },
            { "round(3.5)", 4m },
            { "round(3.7)", 4m },
            { "round(-3.2)", -3m },
            { "round(-3.5)", -4m },
            { "round(-3.7)", -4m },
            { "ceil(3.2)", 4m },
            { "ceil(3.0)", 3m },
            { "ceil(-3.2)", -3m },
            { "ceil(-3.0)", -3m },
            { "floor(3.8)", 3m },
            { "floor(3.0)", 3m },
            { "floor(-3.8)", -4m },
            { "floor(-3.0)", -3m },
        };
    }
}
