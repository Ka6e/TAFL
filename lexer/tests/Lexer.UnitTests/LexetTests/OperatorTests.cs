namespace Lexer.UnitTests.LexetTests;
public class OperatorTests
{
    [Theory]
    [MemberData(nameof(GetArithmeticOperators))]
    public void Can_tokenize_arethetic_operatos(string op, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(op);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetArithmeticOperators()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "+ - * % / **",
                [
                    new Token(TokenType.PlusSign),
                    new Token(TokenType.MinusSign),
                    new Token(TokenType.MultiplySign),
                    new Token(TokenType.ModSign),
                    new Token(TokenType.DivideSign),
                    new Token(TokenType.Exponent)
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetLogicOperators))]
    public void Can_tokenize_logical_operators(string op, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(op);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetLogicOperators()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "|| && !",
                [
                    new Token(TokenType.LogicalOr),
                    new Token(TokenType.LogicalAnd),
                    new Token(TokenType.LogicalNot)
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetCompressionOperators))]
    public void Can_tokenize_compression_operators(string op, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(op);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetBitwiseOperators()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "& | ^ ~",
                [
                    new Token(TokenType.BitwiseAnd),
                    new Token(TokenType.BitwiseOr),
                    new Token(TokenType.BitwiseXOR),
                    new Token(TokenType.BitwiseNot),
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetBitwiseOperators))]
    public void Can_tokenize_bitwise_operators(string op, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(op);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetCompressionOperators()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "== != < <= >= >",
                [
                    new Token(TokenType.Equal),
                    new Token(TokenType.LogicalNotEqual),
                    new Token(TokenType.LessThan),
                    new Token(TokenType.LessThanOrEqual),
                    new Token(TokenType.GreaterThanOrEqual),
                    new Token(TokenType.GreaterThan),
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetFunctionalAndSpecialOperators))]
    public void Can_tokenize_functional_and_special_operators(string op, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(op);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetFunctionalAndSpecialOperators()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "++ -- .. -> : =",
                [
                    new Token(TokenType.Increment),
                    new Token(TokenType.Dicrement),
                    new Token(TokenType.Range),
                    new Token(TokenType.FuncType),
                    new Token(TokenType.Annotation),
                    new Token(TokenType.Assign),
                ]
            },
        };
    }
}
