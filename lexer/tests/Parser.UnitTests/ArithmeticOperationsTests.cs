using Execution;

namespace Parser.UnitTests.ArithemticTests;
public class ArithmeticOperationsTests
{
    private const int Precision = 5;
    private readonly Context context;
    private readonly FakeEnvironment environment;

    public ArithmeticOperationsTests()
    {
        context = new Context();
        environment = new FakeEnvironment();
    }

    [Theory]
    [MemberData(nameof(GetDivisionByZeroData))]
    public void Handle_Divide_By_Zero(string expression)
    {
        Parser parser = new Parser(context, environment, expression);

        Assert.Throws<DivideByZeroException>(() => parser.ParseProgram());
    }

    public static TheoryData<string> GetDivisionByZeroData()
    {
        return new TheoryData<string>()
        {
            { "10 / 0;" },
            { "10 % 0;" },
        };
    }

    [Theory]
    [MemberData(nameof(GetArithmeticOperations))]
    public void Handle_Arithmetic_Operations(string expression, decimal expected)
    {
        Parser parser = new Parser(context, environment, expression);
        parser.ParseProgram();
        decimal result = Assert.Single(environment.Results);

        Assert.Equal(expected, result, Precision);
    }

    public static TheoryData<string, decimal> GetArithmeticOperations()
    {
        return new TheoryData<string, decimal>()
        {
            { "123;", 123m },
            { "456.789;", 456.789m },
            { "5 % 3;", 2m },
            { "19 % 7 % 2;", 1m },
            { "+3;", 3m },
            { "+3.14;", 3.14m },
            { "2 * +3;", 6m },
            { "2 * -3;", -6m },
            { "2 ** 3 ** 2;", 512m },
            { "-2 + 5 + 5;", 8m },
            { "-2 - 2 - 2;", -6m },
            { "4 * 2 / 4 * 0;", 0m },
            { "4 / 2 / 4;", 0.5m },
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
