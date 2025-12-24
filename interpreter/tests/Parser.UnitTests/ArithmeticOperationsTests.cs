using Execution;

using Interpreter;

namespace Parser.UnitTests.ArithemticTests;
public class ArithmeticOperationsTests
{
    private readonly FakeEnvironment environment;
    private readonly GlacierInterpreter interpreter;

    public ArithmeticOperationsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetDivisionByZeroData))]
    public void Handle_Divide_By_Zero(string expression)
    {
        Assert.Throws<DivideByZeroException>(() => interpreter.Execute(expression));
    }

    public static TheoryData<string> GetDivisionByZeroData()
    {
        return new TheoryData<string>()
        {
            { "module main \n10 / 0;" },
            { "module main \n10 % 0;" },
        };
    }

    [Theory]
    [MemberData(nameof(GetArithmeticOperations))]
    public void Handle_Arithmetic_Operations(string expression, decimal expected)
    {
        string code = "module Main \n" + expression;

        interpreter.Execute(code);

        Assert.Equal([expected], environment.Results);
    }

    public static TheoryData<string, decimal> GetArithmeticOperations()
    {
        return new TheoryData<string, decimal>()
        {
            { "123;", 123m },
            { "5 % 3;", 2m },
            { "19 % 7 % 2;", 1m },
            { "+3;", 3m },
            { "2 * +3;", 6m },
            { "2 * -3;", -6m },
            { "2 ** 3 ** 2;", 512m },
            { "-2 + 5 + 5;", 8m },
            { "-2 - 2 - 2;", -6m },
            { "4 * 2 / 4 * 0;", 0m },
            { "4 / 2 / 4;", 0m },
            { "4 % 4 % 4;", 0m },
            { "2 + 3 * 4;", 14m },
            { "10 - 8 / 2;", 6m },
            { "2 * 3 ** 2;",  18m },
            { "(2 + 3) * 4;", 20m },
            { "-2 ** 3;", -8m },
            { "(-1) ** 2;", 1m },
            { "-1 ** 2;", -1m },
        };
    }
}
