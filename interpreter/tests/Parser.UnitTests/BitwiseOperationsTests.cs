using Interpreter;

namespace Parser.UnitTests;
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
    public void Handle_Bitwise_Operations(string expression, decimal expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        decimal result = Assert.Single(environment.Results);

        Assert.Equal(expected, result, Precision);
    }

    public static TheoryData<string, decimal> GetBitwiseOperationsData()
    {
        return new TheoryData<string, decimal>()
        {
            { "5 & 3;", 1m },
            { "7 & 2;", 2m },
            { "0 & 5;", 0m },
            { "5 | 3;", 7m },
            { "4 | 1;", 5m },
            { "5 ^ 3;", 6m },
            { "7 ^ 2;", 5m },
            { "0 ^ 0;", 0m },
            { "~0;", -1m },
            { "~1;", -2m },
            { "~5;", -6m },
            { "~2 | 1;", -3m },
            { "~0 | 1;", -1m },
            { "~5 & 3;", 2m },
            { "~4 ^ 1;", -6m },
        };
    }
}
