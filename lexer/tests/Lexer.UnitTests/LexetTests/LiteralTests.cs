namespace Lexer.UnitTests.LexetTests;
public class LiteralTests
{
    [Theory]
    [MemberData(nameof(GetNumericLiteral))]
    public void Can_tokenize_numeric_literal(string code, List<Token> expected)
    {
        List<Token> tokens = LexerTest.Tokenize(code);
        Assert.Equal(expected, tokens);
    }

    public static TheoryData<string, List<Token>> GetNumericLiteral()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "42 0x2A 0b1010 1000000",
                [
                    new Token(TokenType.Integer, new TokenValue(42)),
                    new Token(TokenType.Integer, new TokenValue(42)),
                    new Token(TokenType.Integer, new TokenValue(10)),
                    new Token(TokenType.Integer, new TokenValue(1000000))
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetDecimalLiteral))]
    public void Can_tokenize_decimal_literal(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetDecimalLiteral()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "3.14 123.456 0.00001",
                [
                    new Token(TokenType.Float, new TokenValue(3.14m)),
                    new Token(TokenType.Float, new TokenValue(123.456m)),
                    new Token(TokenType.Float, new TokenValue(0.00001m))
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetStringLiteral))]
    public void Can_tokenize_string_literal(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetStringLiteral()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "\"Hello, world\" \"Привет\" \"String with spaces\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("Hello, world")),
                    new Token(TokenType.StringLiteral, new TokenValue("Привет")),
                    new Token(TokenType.StringLiteral, new TokenValue("String with spaces"))
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetStringLiteralWithEscapeSequence))]
    public void Can_tokenize_string_literal_with_escape_sequence(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetStringLiteralWithEscapeSequence()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "\"Line1\nLine2\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("Line1\nLine2"))
                ]
            },
            {
                "\"\tSeparated\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("\tSeparated"))
                ]
            },
            {
                "\"It\\'s fine\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("It's fine"))
                ]
            },
            {
                "\"I say, \\\"Hello!\\\"\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("I say, \"Hello!\""))
                ]
            },
            {
                "\"Path: C:\\\\Programm\\\\App\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("Path: C:\\Programm\\App"))
                ]
            },
            {
                "\"Line1\rLine2\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("Line1\rLine2"))
                ]
            },
            {
                "\"Line1\\nLine2\\tTabbed\\rCarriage\"",
                [
                    new Token(TokenType.StringLiteral, new TokenValue("Line1\nLine2\tTabbed\rCarriage"))
                ]
            },
            {
                "\"Unterminated string",
                [
                    new Token(TokenType.Error, new TokenValue("Unterminated string"))
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetBooleanLiteral))]
    public void Can_tonenize_boolean_literal(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetBooleanLiteral()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "true false",
                [
                    new Token(TokenType.True),
                    new Token(TokenType.False)
                ]
            },
        };
    }

    [Fact]
    public void Can_tokenize_null_literal()
    {
        string code = "null";
        Lexer lexer = new Lexer(code);
        Token token = lexer.ParseToken();

        Assert.Equal(new Token(TokenType.NullLiteral), token);
    }

    [Fact]
    public void Can_tokenize_multiline_string()
    {
        string multiLineString = "\"\"\"it's multiline string\"\"\"";
        Lexer lexer = new Lexer(multiLineString);
        Token token = lexer.ParseToken();

        Assert.Equal(new Token(TokenType.StringLiteral, new TokenValue("""it's multiline string""")), token);
    }

    [Theory]
    [MemberData(nameof(GetRawString))]
    public void Can_tokenize_raw_string(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetRawString()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                @"`C:\path\file`",
                [
                    new Token(TokenType.StringLiteral, new TokenValue(@"C:\path\file"))
                ]
            },
        };
    }
}
