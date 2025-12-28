using Interpreter;

using Runtime;

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
    public void Handle_Arithmetic_Operations(string expression, Value expected)
    {
        string code = $"module Main \n print({expression});";

        interpreter.Execute(code);

        Assert.Single(environment.Results);
        Assert.Equal(expected.AsInt(), environment.Results[0].AsInt());
    }

    public static TheoryData<string, Value> GetArithmeticOperations()
    {
        return new TheoryData<string, Value>()
        {
            { "123", new Value(123) },
            { "5 % 3", new Value(2) },
            { "19 % 7 % 2", new Value(1) },
            { "+3", new Value(3) },
            { "+5", new Value(5) },
            { "2 * +3", new Value(6) },
            { "2 * -3", new Value(-6) },
            { "2 ** 3 ** 2", new Value(512) },
            { "-2 + 5 + 5", new Value(8) },
            { "-2 - 2 - 2", new Value(-6) },
            { "4 * 2 / 4 * 0", new Value(0) },
            { "4 / 2 / 4", new Value(0) },
            { "4 % 4 % 4", new Value(0) },
            { "2 + 3 * 4", new Value(14) },
            { "10 - 8 / 2", new Value(6) },
            { "2 * 3 ** 2",  new Value(18) },
            { "(2 + 3) * 4", new Value(20) },
            { "-2 ** 3", new Value(-8) },
            { "(-1) ** 2", new Value(1) },
            { "-1 ** 2", new Value(-1) },
        };
    }

    [Theory]
    [MemberData(nameof(GetArithmeticFloatOperations))]
    public void Handle_Arithmetic_Float_Operations(string expression, Value expected)
    {
        string code = $"module Main \n print({expression});";

        interpreter.Execute(code);

        Assert.Single(environment.Results);
        Assert.Equal(expected.AsFloat(), environment.Results[0].AsFloat() );
    }

    public static TheoryData<string, Value> GetArithmeticFloatOperations()
    {
        return new TheoryData<string, Value>()
        {
            { "456.789", new Value(456.789m) },
            { "+3.14", new Value(3.14m) },
        };
    }
}
