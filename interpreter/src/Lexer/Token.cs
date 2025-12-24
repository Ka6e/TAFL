using System.Text;

namespace Lexer;
public class Token(
        TokenType type,
        TokenValue? value = null
    )
{
    public TokenType Type { get; } = type;

    public TokenValue? Value { get; } = value;

    public override bool Equals(object? obj)
    {
        if (obj is Token other)
        {
            return Type == other.Type && Equals(Value, other.Value);
        }

        return false;
    }

    public override int GetHashCode()
    {
        return HashCode.Combine((int)Type, Value);
    }

    public override string ToString()
    {
        StringBuilder sb = new();
        sb.Append(Type.ToString());
        if (Value != null)
        {
            sb.Append($"({Value})");
        }

        return sb.ToString();
    }
}
