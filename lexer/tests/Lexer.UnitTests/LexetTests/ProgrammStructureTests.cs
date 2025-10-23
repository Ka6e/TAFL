namespace Lexer.UnitTests.LexetTests;
public class ProgrammStructureTests
{
    [Theory]
    [MemberData(nameof(GetConditionalStructure))]
    public void Can_tokenize_conditional_constructions(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetConditionalStructure()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "if x > 10 then (x+5) else (x-5)",
                [
                    new Token(TokenType.If),
                    new Token(TokenType.Identifier, new TokenValue("x")),
                    new Token(TokenType.GreaterThan),
                    new Token(TokenType.Integer, new TokenValue(10)),
                    new Token(TokenType.Then),
                    new Token(TokenType.LParenthesis),
                    new Token(TokenType.Identifier, new TokenValue("x")),
                    new Token(TokenType.PlusSign),
                    new Token(TokenType.Integer, new TokenValue(5)),
                    new Token(TokenType.RParenthesis),
                    new Token(TokenType.Else),
                    new Token(TokenType.LParenthesis),
                    new Token(TokenType.Identifier, new TokenValue("x")),
                    new Token(TokenType.MinusSign),
                    new Token(TokenType.Integer, new TokenValue(5)),
                    new Token(TokenType.RParenthesis),
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetCycleStructure))]
    public void Can_tokenize_circyle_conditions(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetCycleStructure()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "for i := 0; i < 5; i++ { print(i) }",
                [
                    new Token(TokenType.For),
                    new Token(TokenType.Identifier, new TokenValue("i")),
                    new Token(TokenType.Assign),
                    new Token(TokenType.Integer, new TokenValue(0)),
                    new Token(TokenType.Semicolon),
                    new Token(TokenType.Identifier, new TokenValue("i")),
                    new Token(TokenType.LessThan),
                    new Token(TokenType.Integer, new TokenValue(5)),
                    new Token(TokenType.Semicolon),
                    new Token(TokenType.Identifier, new TokenValue("i")),
                    new Token(TokenType.Increment),
                    new Token(TokenType.LBrace),
                    new Token(TokenType.Identifier, new TokenValue("print")),
                    new Token(TokenType.LParenthesis),
                    new Token(TokenType.Identifier, new TokenValue("i")),
                    new Token(TokenType.RParenthesis),
                    new Token(TokenType.RBrace)
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetFunctionType))]
    public void Can_tokenize_func_type(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetFunctionType()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                "func add(a: int, b: int) -> int { return a + b }",
                [
                    new Token(TokenType.Func),
                    new Token(TokenType.Identifier, new TokenValue("add")),
                    new Token(TokenType.LParenthesis),
                    new Token(TokenType.Identifier, new TokenValue("a")),
                    new Token(TokenType.Annotation),
                    new Token(TokenType.IntegerType),
                    new Token(TokenType.Comma),
                    new Token(TokenType.Identifier, new TokenValue("b")),
                    new Token(TokenType.Annotation),
                    new Token(TokenType.IntegerType),
                    new Token(TokenType.RParenthesis),
                    new Token(TokenType.FuncType),
                    new Token(TokenType.IntegerType),
                    new Token(TokenType.LBrace),
                    new Token(TokenType.Return),
                    new Token(TokenType.Identifier, new TokenValue("a")),
                    new Token(TokenType.PlusSign),
                    new Token(TokenType.Identifier, new TokenValue("b")),
                    new Token(TokenType.RBrace)
                ]
            },
        };
    }
}
