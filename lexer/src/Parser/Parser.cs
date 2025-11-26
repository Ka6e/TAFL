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
        context.PushScope(new Scope());
        try
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
        finally
        {
            context.PopScope();
        }
    }

    private decimal ParseTopLevelStatement()
    {
        return ParseStatement();
    }

    private decimal ParseStatement()
    {
        Token t = tokens.Peek();
        switch (t.Type)
        {
            case TokenType.Let:
            case TokenType.Var:
                return ParseVariableDecl();
            default:
                return ParseExpressionStatement();
        }
    }

    private decimal ParseExpressionStatement()
    {
        decimal value = ParseExpression();
        Match(TokenType.Semicolon);

        return value;
    }

    // todo: убрать 
    // todo: прописать все грамматики для всех функций
    private decimal ParseVariableDecl()
    {
        switch (tokens.Peek().Type)
        {
            case TokenType.Let:
                return ParseConstantDefinition();
            case TokenType.Var:
                return ParseVariableDefinition();
            default:
                // todo: бросать исключение 
                throw new Exception("Unexpected lexeme");
        }
    }

    private decimal ParseVariableDefinition()
    {
        Match(TokenType.Var);
        string name = Match(TokenType.Identifier).Value!.ToString();
        decimal value = 0;
        if (tokens.Peek().Type == TokenType.Assign)
        {
            Match(TokenType.Assign);
            value = ParseExpression();
        }

        context.DefineVariable(name, value);

        return value;
    }

    private decimal ParseConstantDefinition()
    {
        Match(TokenType.Let);
        string name = Match(TokenType.Identifier).Value!.ToString();
        Match(TokenType.Annotation);
        string type = ParseTypeAnnotation();
        tokens.Advance();

        decimal value = 0;
        if (tokens.Peek().Type == TokenType.Assign)
        {
            Match(TokenType.Assign);
            value = ParseExpression();
        }

        context.DefineConstant(name, value);

        return value;
    }

    private string ParseTypeAnnotation()
    {
        Token t = tokens.Peek();
        switch (t.Type)
        {
            case TokenType.IntegerType:
                return "int";
            case TokenType.FloatType:
                return "float";
            case TokenType.Identifier:
                return t.Value!.ToString();
            default:
                throw new UnexpectedLexemeException(t.Type, t);
        }
    }

    private decimal ParseExpression()
    {
        return ParseLogicalOrExpression();
    }

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
                string name = t.Value!.ToString();
                if (tokens.Peek().Type == TokenType.LParenthesis)
                {
                    tokens.Advance();
                    List<decimal> args = new List<decimal>();
                    if (tokens.Peek().Type != TokenType.RParenthesis)
                    {
                        args = ParseExpressionList();
                    }

                    tokens.Advance();
                    return BuiltinFunctions.Invoke(name, args);
                }
                else if (tokens.Peek().Type == TokenType.Assign)
                {
                    tokens.Advance();
                    decimal result = ParseExpression();
                    context.AssignVariable(name, result);
                    return result;
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
                            return context.GetValue(t.Value.ToString());
                    }
                }

            case TokenType.LParenthesis:
                tokens.Advance();
                decimal value = ParseExpression();
                if (tokens.Peek().Type != TokenType.RParenthesis)
                {
                    throw new UnexpectedLexemeException(tokens.Peek().Type, t);
                }

                tokens.Advance();
                return value;
            default:
                throw new UnexpectedLexemeException(TokenType.Integer, t);
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
            throw new UnexpectedLexemeException(expected, t);
        }

        tokens.Advance();

        return t;
    }
}
