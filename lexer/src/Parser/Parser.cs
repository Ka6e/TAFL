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

    /// <summary>
    /// program = module_decl, { import_decl }, { top_level_decl } ;.
    /// </summary>
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

    /// <summary>
    /// top_level_decl = class_decl
    ///                | enum_decl
    ///                | function_decl
    ///                | statement ;.
    /// </summary>
    private decimal ParseTopLevelStatement()
    {
        return ParseStatement();
    }

    /// <summary>
    /// statement = variable_decl
    ///           | assignment
    ///           | if_statement
    ///           | for_statement
    ///           | return_statement
    ///           | match_statement
    ///           | expression_statement ;.
    /// </summary>
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

    /// <summary>
    /// expression_statement = expression, ";" ;.
    /// </summary>
    private decimal ParseExpressionStatement()
    {
        decimal value = ParseExpression();
        Match(TokenType.Semicolon);

        return value;
    }

    /// <summary>
    /// variable_decl = let_variable_decl | var_variable_decl;.
    /// </summary>
    private decimal ParseVariableDecl()
    {
        switch (tokens.Peek().Type)
        {
            case TokenType.Let:
                return ParseLetVariableDecl();
            case TokenType.Var:
                return ParseVarVariableDecl();
            default:
                throw new UnexpectedLexemeException(TokenType.Let | TokenType.Var, tokens.Peek());
        }
    }

    /// <summary>
    /// var_variable_decl = "var, identifier, [ "=", expression ], ";" ;.
    /// </summary>
    private decimal ParseVarVariableDecl()
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

    /// <summary>
    /// let_variable_decl = "let", identifier, ":", type_annotation, [ "=", expression ], ";" ;.
    /// </summary>
    private decimal ParseLetVariableDecl()
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

    /// <summary>
    /// expressions_list = expression, { ",", expression } ;.
    /// </summary>
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

    /// <summary>
    /// expression = logical_or_expression ;.
    /// </summary>
    private decimal ParseExpression()
    {
        return ParseLogicalOrExpression();
    }

    /// <summary>
    /// logical_or_expression  = logical_and_expression, { "||", logical_and_expression } ;.
    /// </summary>
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

    /// <summary>
    /// logical_and_expression = equality_expression, { "&&", equality_expression } ;.
    /// </summary>
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

    /// <summary>
    /// equality_expression = bitwise_or_expression, { ("==" | "!=" | ".<" | "<=" | ">" | ">="), bitwise_or_expression } ;
    /// </summary>
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

    /// <summary>
    /// bitwise_or_expression = bitwise_xor_expression, { "|", bitwise_xor_expression } ;.
    /// </summary>
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

    /// <summary>
    /// bitwise_xor_expression = bitwise_and_expression, { "^", bitwise_and_expression } ;.
    /// </summary>
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

    /// <summary>
    /// bitwise_and_expression = additive_expression, { "&", additive_expression } ;.
    /// </summary>
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

    /// <summary>
    /// additive_expression = multiplicative_expression, { ("+" | "-"), multiplicative_expression } ;.
    /// </summary>
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

    /// <summary>
    /// multiplicative_expression = unary_expression, { ("*" | "/" | "%" ), unary_expression } ;.
    /// </summary>
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

    /// <summary>
    /// power_expression = primary_expression, { "**", unary_expression } ;.
    /// </summary>
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

    /// <summary>
    /// unary_expression = [ "+" | "-" | "~" | "!" ], power_expression ;.
    /// </summary>
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

    /// <summary>
    /// primary_expression = number
    ///                    | identifier
    ///                    | function_call
    ///                    | "(", expression, ")" ;.
    /// </summary>
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

                    Match(TokenType.RParenthesis);
                    if (name == "readNumber")
                    {
                        return environment.ReadNumber();
                    }

                    if (name == "print")
                    {
                        if (args.Count != 1)
                        {
                            throw new ArgumentException("print() expects exactly one argument");
                        }

                        environment.WriteNumber(args[0]);
                        return args[0];
                    }

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
