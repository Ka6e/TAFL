using Execution;

namespace Parser.UnitTests;
public class ErrorsTests
{
    private readonly Context context;
    private readonly FakeEnvironment environment;

    public ErrorsTests()
    {
        context = new Context();
        environment = new FakeEnvironment();
    }

    [Theory]
    [MemberData(nameof(GetErrors))]
    public void Handle_Invalid_Expression(string expression)
    {
        Parser parser = new Parser(context, environment, expression);

        Assert.Throws<UnexpectedLexemeException>(() => parser.ParseProgram());
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
        string expression = "max()";
        Parser parser = new Parser(context, environment, expression);

        Assert.Throws<ArgumentException>(() => parser.ParseProgram());
    }
}
