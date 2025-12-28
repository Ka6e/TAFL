using Interpreter;

using Runtime;

namespace Parser.UnitTests.BuiltinFunctionsTests;
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
    public void Handle_Built_In_Functions(string expression, Value expected)
    {
        string code = $"module Main\n print({expression});";

        interpreter.Execute(code);

        Assert.Equal(expected.AsFloat(), environment.Results[0].AsFloat());
    }

    public static TheoryData<string, Value> GetBuiltinFunctions()
    {
        return new TheoryData<string, Value>()
        {
            { "abs(-10)", new Value(10m) },
            { "abs(-7.5)", new Value(7.5m) },
            { "min(10, 4, 1, 56)", new Value(1m) },
            { "min(-1, -50, 0)", new Value(-50m) },
            { "max(1, 2, 3, 4, 5)", new Value(5m) },
            { "max(-10, -8, -1)", new Value(-1m) },
            { "pow(-1, 2)", new Value(1m) },
            { "pow(1, 2)", new Value(1m) },
            { "pow(2, 3)", new Value(8m) },
            { "pow(-2, 3)", new Value(-8m) },
            { "round(3.2)", new Value(3m) },
            { "round(3.5)", new Value(4m) },
            { "round(3.7)", new Value(4m) },
            { "round(-3.2)", new Value(-3m) },
            { "round(-3.5)", new Value(-4m) },
            { "round(-3.7)", new Value(-4m) },
            { "ceil(3.2)", new Value(4m) },
            { "ceil(3.0)", new Value(3m) },
            { "ceil(-3.2)", new Value(-3m) },
            { "ceil(-3.0)", new Value(-3m) },
            { "floor(3.8)", new Value(3m) },
            { "floor(3.0)", new Value(3m) },
            { "floor(-3.8)", new Value(-4m) },
            { "floor(-3.0)", new Value(-3m) },
        };
    }
}
