namespace Lexer.UnitTests.LexetTests;
public class KeyWordsTests
{
    [Theory]
    [MemberData(nameof(GetKeyWords))]
    public void Can_tokenize_keywords(string keyWords, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(keyWords);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetKeyWords()
    {
        return new TheoryData<string, List<Token>>()
        {
            { "module", [ new Token(TokenType.Module),] },
            { "import", [ new Token(TokenType.Import),] },
            { "new", [ new Token(TokenType.New),] },
            { "func", [ new Token(TokenType.Func),] },
            { "let var", [ new Token(TokenType.Let), new Token(TokenType.Var),] },
            { "enum", [ new Token(TokenType.Enum),] },
            { "interface", [ new Token(TokenType.Interface),] },
            { "if then else", [ new Token(TokenType.If), new Token(TokenType.Then), new Token(TokenType.Else),] },
            { "for", [ new Token(TokenType.For),] },
            { "return", [ new Token(TokenType.Return),] },
            { "throw try catch", [ new Token(TokenType.Throw), new Token(TokenType.Try), new Token(TokenType.Catch),] },
            { "type", [ new Token(TokenType.Type),] },
            { "where", [ new Token(TokenType.Where),] },
            { "implements", [ new Token(TokenType.Implements),] },
            { "this", [ new Token(TokenType.This),] },
            { "true", [ new Token(TokenType.True),] },
            { "false", [ new Token(TokenType.False),] },
            { "null", [ new Token(TokenType.NullLiteral),] },
            { "int", [ new Token(TokenType.IntegerType),] },
            { "string", [ new Token(TokenType.StringType),] },
            { "char", [ new Token(TokenType.CharType),] },
            { "float", [ new Token(TokenType.FloatType),] },
        };
    }
}
