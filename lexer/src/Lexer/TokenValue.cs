using System.Globalization;

namespace Lexer;
public class TokenValue
{
    private readonly object _value;

    public TokenValue(string value)
    {
        _value = value;
    }

    public TokenValue(decimal value)
    {
        _value = value;
    }

    public override string ToString()
    {
        return _value switch
        {
            string s => s,
            decimal d => d.ToString(CultureInfo.InvariantCulture),
            _ => throw new NotImplementedException()
        };
    }

    public decimal ToDecimal()
    {
        return _value switch
        {
            string s => decimal.Parse(s, CultureInfo.InvariantCulture),
            decimal d => d,
            _ => throw new NotImplementedException()
        };
    }

    public override bool Equals(object? obj)
    {
        if (obj is TokenValue other)
        {
            return _value switch
            {
                string s => (string)other._value == s,
                decimal d => (decimal)other._value == d,
                _ => throw new NotImplementedException()
            };
        }

        return false;
    }

    public override int GetHashCode()
    {
        return _value.GetHashCode();
    }
}
