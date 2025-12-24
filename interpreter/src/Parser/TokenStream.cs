using Lexer;

namespace Parser;
public class TokenStream
{
    private readonly Lexer.Lexer lexer;
    private Token nextToken;

    public TokenStream(string code)
    {
        lexer = new Lexer.Lexer(code);
        nextToken = lexer.ParseToken();
    }

    public Token Peek()
    {
        return nextToken;
    }

    public void Advance()
    {
        nextToken = lexer.ParseToken();
    }
}
