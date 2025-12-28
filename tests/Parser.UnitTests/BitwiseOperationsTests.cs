using Interpreter;

using Runtime;

namespace Parser.UnitTests.BitwiseOperationsTests;
public class BitwiseOperationsTests
{
    private const int Precision = 5;
    private readonly GlacierInterpreter interpreter;
    private readonly FakeEnvironment environment;

    public BitwiseOperationsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetBitwiseOperationsData))]
    public void Handle_Bitwise_Operations(string expression, Value expected)
    {
        string code = $"module Main\n print({expression});";

        interpreter.Execute(code);

        Assert.Equal(expected.AsInt(), environment.Results[0].AsInt());
    }

    public static TheoryData<string, Value> GetBitwiseOperationsData()
    {
        return new TheoryData<string, Value>()
        {
            { "5 & 3", new Value(1) },
            { "7 & 2", new Value(2) },
            { "0 & 5", new Value(0) },
            { "5 | 3", new Value(7) },
            { "4 | 1", new Value(5) },
            { "5 ^ 3", new Value(6) },
            { "7 ^ 2", new Value(5) },
            { "0 ^ 0", new Value(0) },
            { "~0", new Value(-1) },
            { "~1", new Value(-2) },
            { "~5", new Value(-6) },
            { "~2 | 1", new Value(-3) },
            { "~0 | 1", new Value(-1) },
            { "~5 & 3", new Value(2) },
            { "~4 ^ 1", new Value(-6) },
        };
    }
}
