using Lexer;

namespace Parser;

#pragma warning disable RCS1194
public class UnexpectedLexemException : Exception
{
    public UnexpectedLexemException(TokenType expected, Token actual)
        : base($"Unexpected lexeme {actual} where expected {expected}")
    {
    }
}
#pragma warning restore RCS1194
