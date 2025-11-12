namespace Parser.UnitTests;
public class ErrorsTests
{
    [Theory]
    [MemberData(nameof(GetErrors))]
    public void Handle_Invalid_Expression(string expression)
    {
        Assert.Throws<UnexpectedLexemException>(() => Parser.EvaluateExpression(expression));
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

        Assert.Throws<ArgumentException>(() => Parser.EvaluateExpression(expression));
    }
}
