namespace Lexer.UnitTests.LexetTests;
public static class LexerTest
{
    public static List<Token> Tokenize(string code)
    {
        List<Token> results = new();
        Lexer lexer = new(code);

        for (Token t = lexer.ParseToken(); t.Type != TokenType.EndOfFile; t = lexer.ParseToken())
        {
            results.Add(t);
        }

        return results;
    }
}
