using Interpreter;

namespace Parser.UnitTests.StatementsTests;
public class StatementsTests
{
    private readonly FakeEnvironment environment;
    private readonly GlacierInterpreter interpreter;

    public StatementsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetStatementsPositiveData))]
    public void Parse_Statements_With_Positive_Cases(string expression, decimal[] expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetStatementsPositiveData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "var sum = 0;" +
                "for i = 0, i < 10, i = i + 1 in {" +
                "sum = sum + 1;" +
                "}" +
                "sum;", [10]
            },
        };
    }
}
