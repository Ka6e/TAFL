using Execution;

using Lexer;

namespace Parser;
public class Parser
{
    private readonly TokenStream tokens;
    private readonly Context context;
    private readonly IEnvironment environment;

    public Parser(Context context, IEnvironment environment, string code)
    {
        this.context = context;
        this.environment = environment;
        tokens = new TokenStream(code);
    }

    public void ParseProgram()
    {
        do
        {
            decimal result = ParseTopLevelStatement();

            if (tokens.Peek().Type == TokenType.Semicolon)
            {
                Match(TokenType.Semicolon);
            }

            environment.AddResult(result);
        }
        while (tokens.Peek().Type != TokenType.EndOfFile);

    }

    private decimal ParseTopLevelStatement()
    {
        return ParseStatement();
    }

    private decimal ParseStatement()
    {

    }

    public static decimal EvaluateExpression(string code)
    {
        Parser parser = new Parser(code);

        decimal result = parser.ParseExpression();

        if (parser.tokens.Peek().Type != TokenType.EndOfFile)
        {
            throw new UnexpectedLexemException(TokenType.EndOfFile, parser.tokens.Peek());
        }

        return result;
    }

    private decimal ParseExpression() => ParseLogicalOrExpression();

    private decimal ParseLogicalOrExpression()
    {
        decimal value = ParseLogicalAndExpression();
        while (tokens.Peek().Type == TokenType.LogicalOr)
        {
            tokens.Advance();
            decimal right = ParseLogicalAndExpression();
            value = (value != 0 || right != 0) ? 1 : 0;
        }

        return value;
    }

    private decimal ParseLogicalAndExpression()
    {
        decimal value = ParseEqualityExpression();
        while (tokens.Peek().Type == TokenType.LogicalAnd)
        {
            tokens.Advance();
            decimal right = ParseEqualityExpression();
            value = (value != 0 && right != 0) ? 1 : 0;
        }

        return value;
    }

    private decimal ParseEqualityExpression()
    {
        decimal value = ParseBitwiseOrExpression();
        while (true)
        {
            switch (tokens.Peek().Type)
            {
                case TokenType.Equal:
                    tokens.Advance();
                    value = (value == ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                case TokenType.LogicalNotEqual:
                    tokens.Advance();
                    value = (value != ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                case TokenType.LessThan:
                    tokens.Advance();
                    value = (value < ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                case TokenType.LessThanOrEqual:
                    tokens.Advance();
                    value = (value <= ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                case TokenType.GreaterThan:
                    tokens.Advance();
                    value = (value > ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                case TokenType.GreaterThanOrEqual:
                    tokens.Advance();
                    value = (value >= ParseBitwiseOrExpression()) ? 1 : 0;
                    break;
                default:
                    return value;
            }
        }
    }

    private decimal ParseBitwiseOrExpression()
    {
        decimal value = ParseBitwiseXorExpression();
        while (tokens.Peek().Type == TokenType.BitwiseOr)
        {
            tokens.Advance();
            value = (decimal)((long)value | (long)ParseBitwiseXorExpression());
        }

        return value;
    }

    private decimal ParseBitwiseXorExpression()
    {
        decimal value = ParseBitwiseAndExpression();
        while (tokens.Peek().Type == TokenType.BitwiseXOR)
        {
            tokens.Advance();
            value = (decimal)((long)value ^ (long)ParseBitwiseAndExpression());
        }

        return value;
    }

    private decimal ParseBitwiseAndExpression()
    {
        decimal value = ParseAdditiveExpression();
        while (tokens.Peek().Type == TokenType.BitwiseAnd)
        {
            tokens.Advance();
            value = (decimal)((long)value & (long)ParseAdditiveExpression());
        }

        return value;
    }

    private decimal ParseAdditiveExpression()
    {
        decimal value = ParseMultiplicativeExpression();
        while (true)
        {
            switch (tokens.Peek().Type)
            {
                case TokenType.PlusSign:
                    tokens.Advance();
                    value += ParseMultiplicativeExpression();
                    break;
                case TokenType.MinusSign:
                    tokens.Advance();
                    value -= ParseMultiplicativeExpression();
                    break;
                default:
                    return value;
            }
        }
    }

    private decimal ParseMultiplicativeExpression()
    {
        decimal value = ParseUnaryExpression();
        while (true)
        {
            switch (tokens.Peek().Type)
            {
                case TokenType.MultiplySign:
                    tokens.Advance();
                    value *= ParseUnaryExpression();
                    break;
                case TokenType.DivideSign:
                    tokens.Advance();
                    value /= ParseUnaryExpression();
                    break;
                case TokenType.ModSign:
                    tokens.Advance();
                    value %= ParseUnaryExpression();
                    break;
                default:
                    return value;
            }
        }
    }

    private decimal ParsePowerExpression()
    {
        decimal value = ParsePrimaryExpression();
        while (tokens.Peek().Type == TokenType.Exponent)
        {
            tokens.Advance();
            decimal exponent = ParseUnaryExpression();
            value = (decimal)Math.Pow((double)value, (double)exponent);
        }

        return value;
    }

    private decimal ParseUnaryExpression()
    {
        switch (tokens.Peek().Type)
        {
            case TokenType.PlusSign:
                tokens.Advance();
                return +ParseUnaryExpression();
            case TokenType.MinusSign:
                tokens.Advance();
                return -ParseUnaryExpression();
            case TokenType.BitwiseNot:
                tokens.Advance();
                return ~(long)ParseUnaryExpression();
            case TokenType.LogicalNot:
                tokens.Advance();
                decimal value = ParseUnaryExpression();
                return value != 0 ? 0 : 1;
            default:
                return ParsePowerExpression();
        }
    }

    private decimal ParsePrimaryExpression()
    {
        Token t = tokens.Peek();
        switch (t.Type)
        {
            case TokenType.Integer:
            case TokenType.Float:
                tokens.Advance();
                return t.Value!.ToDecimal();
            case TokenType.Identifier:
                tokens.Advance();
                if (tokens.Peek().Type == TokenType.LParenthesis)
                {
                    tokens.Advance();
                    List<decimal> args = new List<decimal>();
                    if (tokens.Peek().Type != TokenType.RParenthesis)
                    {
                        args = ParseExpressionList();
                    }

                    tokens.Advance();
                    return BuiltinFunctions.Invoke(t.Value!.ToString(), args);
                }
                else
                {
                    switch (t.Value!.ToString())
                    {
                        case "Pi":
                            return (decimal)Math.PI;
                        case "Euler":
                            return (decimal)Math.PI;
                        default:
                            throw new Exception($"Unknown identifier: {t.Value}");
                    }
                }

            case TokenType.LParenthesis:
                tokens.Advance();
                decimal value = ParseExpression();
                if (tokens.Peek().Type != TokenType.RParenthesis)
                {
                    throw new UnexpectedLexemException(tokens.Peek().Type, t);
                }

                tokens.Advance();
                return value;
            default:
                throw new UnexpectedLexemException(TokenType.Integer, t);
        }
    }

    private List<decimal> ParseExpressionList()
    {
        List<decimal> values = new()
        {
            ParseExpression(),
        };

        while (tokens.Peek().Type == TokenType.Comma)
        {
            tokens.Advance();
            values.Add(ParseExpression());
        }

        return values;
    }

    private Token Match(TokenType expected)
    {
        Token t = tokens.Peek();

        if (t.Type != expected)
        {
            throw new UnexpectedLexemException(expected, t);
        }

        tokens.Advance();

        return t;
    }
}
