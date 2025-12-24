using Interpreter;

namespace Parser.UnitTests.ErrorsTests;
public class ErrorsTests
{
    private readonly GlacierInterpreter interpreter;
    private readonly FakeEnvironment environment;

    public ErrorsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetErrors))]
    public void Handle_Invalid_Expression(string expression)
    {
        string code = "module Main\n" + expression;

        Assert.Throws<UnexpectedLexemeException>(() => interpreter.Execute(code));
    }

    public static TheoryData<string> GetErrors()
    {
        return new TheoryData<string>()
        {
            { "(1 + 2" },
            { "1 + 2)" },
            { "1 + " },
            { "max(1, 2,)" },
            { "max(1 2)" },
            { ".25" },
        };
    }

    [Fact]
    public void Max_InvalidArguments_Exception()
    {
        string expression = "module main\n max();";

        Assert.Throws<ArgumentException>(() => interpreter.Execute(expression));
    }
}
