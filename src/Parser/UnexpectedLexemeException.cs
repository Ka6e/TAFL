using Lexer;

namespace Parser;

#pragma warning disable RCS1194
public class UnexpectedLexemeException : Exception
{
    public UnexpectedLexemeException(TokenType expected, Token actual)
        : base($"Unexpected lexeme {actual} where expected {expected}")
    {
    }

    public UnexpectedLexemeException(Token actual)
        : base($"Unexpected lexeme {actual.Type}")
    {
    }
}
#pragma warning restore RCS1194
