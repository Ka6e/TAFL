using Ast.Expressions;

using Execution;

using Lexer;

namespace Parser;
public class Parser
{
    private readonly TokenStream tokens;

    public Parser(string code)
    {
        tokens = new TokenStream(code);
    }

    /// <summary>
    /// program = module_decl, { import_decl }, { top_level_decl } ;.
    /// </summary>
    public Expression ParseProgram()
    {
        //context.PushScope(new Scope());
        //try
        //{
        //    do
        //    {
        //        decimal result = ParseTopLevelStatement();

        //        if (tokens.Peek().Type == TokenType.Semicolon)
        //        {
        //            Match(TokenType.Semicolon);
        //        }

        //        environment.AddResult(result);
        //    }
        //    while (tokens.Peek().Type != TokenType.EndOfFile);
        //}
        //finally
        //{
        //    context.PopScope();
        //}
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

    /// <summary>
    ///    type_annotation = "int" | "float" | identifier ;.
    /// </summary>
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

    private Expression ParseFunctionCall(string name)
    {
        Match(TokenType.LParenthesis);

        List<Expression> args = new List<Expression>();
        if (tokens.Peek().Type != TokenType.RParenthesis)
        {
            args.Add(ParseExpression());
            while (tokens.Peek().Type == TokenType.Comma)
            {
                tokens.Advance();
                args.Add(ParseExpression());
            }
        }

        Match(TokenType.RParenthesis);

        return new FunctionCallExpression(name, args);
    }


    /// <summary>
    /// expression = logical_or_expression ;.
    /// </summary>
    private Expression ParseExpression()
    {
        return ParseLogicalOrExpression();
    }

    /// <summary>
    /// logical_or_expression  = logical_and_expression, { "||", logical_and_expression } ;.
    /// </summary>
    private Expression ParseLogicalOrExpression()
    {
        Expression left = ParseLogicalAndExpression();
        while (tokens.Peek().Type == TokenType.LogicalOr)
        {
            tokens.Advance();
            Expression right = ParseLogicalAndExpression();
            left = new BinaryOperationExpression(left, BinaryOperation.Or, right);
        }

        return left;
    }

    /// <summary>
    /// logical_and_expression = equality_expression, { "&&", equality_expression } ;.
    /// </summary>
    private Expression ParseLogicalAndExpression()
    {
        Expression left = ParseEqualityExpression();
        while (tokens.Peek().Type == TokenType.LogicalAnd)
        {
            tokens.Advance();
            Expression right = ParseEqualityExpression();
            left = new BinaryOperationExpression(left, BinaryOperation.And, right);
        }

        return left;
    }

    /// <summary>
    /// equality_expression = bitwise_or_expression, { ("==" | "!=" | ".<" | "<=" | ">" | ">="), bitwise_or_expression } ;
    /// </summary>
    private Expression ParseEqualityExpression()
    {
        Expression left = ParseBitwiseOrExpression();
        if (IsComprassionOperator(tokens.Peek().Type))
        {
            BinaryOperation operation = tokens.Peek().Type switch
            {
                TokenType.Equal => BinaryOperation.Equal,
                TokenType.LogicalNotEqual => BinaryOperation.NotEqual,
                TokenType.LessThan => BinaryOperation.LessThan,
                TokenType.GreaterThan => BinaryOperation.GreaterThan,
                TokenType.LessThanOrEqual => BinaryOperation.LessThanOrEqual,
                TokenType.GreaterThanOrEqual => BinaryOperation.GreaterThanOrEqual,
                _ => throw new UnexpectedLexemeException(tokens.Peek())
            };

            tokens.Advance();
            Expression right = ParseBitwiseOrExpression();

            return new BinaryOperationExpression(left, operation, right);
        }
        //while (true)
        //{
        //    switch (tokens.Peek().Type)
        //    {
        //        case TokenType.Equal:
        //            tokens.Advance();
        //            value = (value == ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        case TokenType.LogicalNotEqual:
        //            tokens.Advance();
        //            value = (value != ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        case TokenType.LessThan:
        //            tokens.Advance();
        //            value = (value < ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        case TokenType.LessThanOrEqual:
        //            tokens.Advance();
        //            value = (value <= ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        case TokenType.GreaterThan:
        //            tokens.Advance();
        //            value = (value > ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        case TokenType.GreaterThanOrEqual:
        //            tokens.Advance();
        //            value = (value >= ParseBitwiseOrExpression()) ? 1 : 0;
        //            break;
        //        default:
        //            return value;
        //    }
        //}

        return left;
    }

    /// <summary>
    /// bitwise_or_expression = bitwise_xor_expression, { "|", bitwise_xor_expression } ;.
    /// </summary>
    private Expression ParseBitwiseOrExpression()
    {
        Expression left = ParseBitwiseXorExpression();
        while (tokens.Peek().Type == TokenType.BitwiseOr)
        {
            tokens.Advance();
            Expression right = ParseBitwiseXorExpression();
            left = new BitwiseOperationExpression(left, BitwiseOperation.Or, right);
        }

        return left;
    }

    /// <summary>
    /// bitwise_xor_expression = bitwise_and_expression, { "^", bitwise_and_expression } ;.
    /// </summary>
    private Expression ParseBitwiseXorExpression()
    {
        Expression left = ParseBitwiseAndExpression();
        while (tokens.Peek().Type == TokenType.BitwiseXOR)
        {
            tokens.Advance();
            Expression right = ParseBitwiseAndExpression();
            left = new BitwiseOperationExpression(left, BitwiseOperation.XOR, right);
            //left = (decimal)((long)left ^ (long)ParseBitwiseAndExpression());
        }

        return left;
    }

    /// <summary>
    /// bitwise_and_expression = additive_expression, { "&", additive_expression } ;.
    /// </summary>
    private Expression ParseBitwiseAndExpression()
    {
        Expression left = ParseAdditiveExpression();
        while (tokens.Peek().Type == TokenType.BitwiseAnd)
        {
            tokens.Advance();
            Expression right = ParseAdditiveExpression();
            left = new BitwiseOperationExpression(left, BitwiseOperation.And, right);
        }

        return left;
    }

    /// <summary>
    /// additive_expression = multiplicative_expression, { ("+" | "-"), multiplicative_expression } ;.
    /// </summary>
    private Expression ParseAdditiveExpression()
    {
        Expression left = ParseMultiplicativeExpression();
        while (true)
        {
            switch (tokens.Peek().Type)
            {
                case TokenType.PlusSign:
                    tokens.Advance();
                    Expression plusRight = ParseMultiplicativeExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Add, plusRight);
                    break;
                case TokenType.MinusSign:
                    tokens.Advance();
                    Expression minusRight = ParseMultiplicativeExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Subtract, minusRight);
                    break;
                default:
                    return left;
            }
        }
    }

    /// <summary>
    /// multiplicative_expression = unary_expression, { ("*" | "/" | "%" ), unary_expression } ;.
    /// </summary>
    private Expression ParseMultiplicativeExpression()
    {
        Expression left = ParseUnaryExpression();
        while (true)
        {
            switch (tokens.Peek().Type)
            {
                case TokenType.MultiplySign:
                    tokens.Advance();
                    Expression multiplyRight = ParseMultiplicativeExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Multiply, multiplyRight);
                    break;
                case TokenType.DivideSign:
                    tokens.Advance();
                    Expression devideRight = ParseMultiplicativeExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Divide, devideRight);
                    break;
                case TokenType.ModSign:
                    tokens.Advance();
                    Expression moduleRight = ParseMultiplicativeExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Module, moduleRight);
                    break;
                default:
                    return left;
            }
        }
    }

    /// <summary>
    /// power_expression = primary_expression, { "**", unary_expression } ;.
    /// </summary>
    private Expression ParsePowerExpression()
    {
        Expression left = ParsePrimaryExpression();
        while (tokens.Peek().Type == TokenType.Exponent)
        {
            tokens.Advance();
            Expression right = ParseUnaryExpression();
            left = new BinaryOperationExpression(left, BinaryOperation.Exponent, right);
        }

        return left;
    }

    /// <summary>
    /// unary_expression = [ "+" | "-" | "~" | "!" ], power_expression ;.
    /// </summary>
    private Expression ParseUnaryExpression()
    {
        switch (tokens.Peek().Type)
        {
            case TokenType.PlusSign:
                tokens.Advance();
                return new UnaryOperationExpression(UnaryOperation.Plus, ParseUnaryExpression());
            case TokenType.MinusSign:
                tokens.Advance();
                return new UnaryOperationExpression(UnaryOperation.Minus, ParseUnaryExpression());
            case TokenType.BitwiseNot:
                tokens.Advance();
                return new UnaryOperationExpression(UnaryOperation.BitwiseNot, ParseUnaryExpression());
            case TokenType.LogicalNot:
                tokens.Advance();
                return new UnaryOperationExpression(UnaryOperation.Not, ParseUnaryExpression());
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
    private Expression ParsePrimaryExpression()
    {
        Token t = tokens.Peek();
        switch (t.Type)
        {
            case TokenType.Integer:
            case TokenType.Float:
                tokens.Advance();
                return new LiteralExpression(new Runtime.Value(t.Value!.ToDecimal()));
            case TokenType.True:
                return new LiteralExpression(new Runtime.Value(true));
            case TokenType.False:
                return new LiteralExpression(new Runtime.Value(false));
            case TokenType.StringLiteral:
                return new LiteralExpression(new Runtime.Value((string)t.Value!.ToString()));
            case TokenType.Identifier:
                string name = Match(TokenType.Identifier).Value!.ToString();
                if (tokens.Peek().Type == TokenType.LParenthesis)
                {
                    return ParseFunctionCall(name);
                }
                else
                {
                    return new VariableExpression(name);
                }

            case TokenType.LParenthesis:
                tokens.Advance();
                Expression expression = ParseExpression();
                Match(TokenType.RParenthesis);
                return expression;
            default:
                throw new UnexpectedLexemeException(t);
                //    tokens.Advance();
                //    string name = t.Value!.ToString();

                //    if (tokens.Peek().Type == TokenType.LParenthesis)
                //    {
                //        tokens.Advance();
                //        List<decimal> args = new List<decimal>();
                //        if (tokens.Peek().Type != TokenType.RParenthesis)
                //        {
                //            args = ParseExpressionList();
                //        }

                //        Match(TokenType.RParenthesis);
                //        if (name == "readNumber")
                //        {
                //            return environment.ReadNumber();
                //        }

                //        if (name == "print")
                //        {
                //            if (args.Count != 1)
                //            {
                //                throw new ArgumentException("print() expects exactly one argument");
                //            }

                //            environment.WriteNumber(args[0]);
                //            return args[0];
                //        }

                //        return BuiltinFunctions.Invoke(name, args);
                //    }
                //    else if (tokens.Peek().Type == TokenType.Assign)
                //    {
                //        tokens.Advance();
                //        decimal result = ParseExpression();
                //        context.AssignVariable(name, result);
                //        return result;
                //    }
                //    else
                //    {
                //        switch (t.Value!.ToString())
                //        {
                //            case "Pi":
                //                return (decimal)Math.PI;
                //            case "Euler":
                //                return (decimal)Math.PI;
                //            default:
                //                return context.GetValue(t.Value.ToString());
                //        }
                //    }

                //case TokenType.LParenthesis:
                //    tokens.Advance();
                //    decimal value = ParseExpression();
                //    if (tokens.Peek().Type != TokenType.RParenthesis)
                //    {
                //        throw new UnexpectedLexemeException(tokens.Peek().Type, t);
                //    }

                //    tokens.Advance();
                //    return value;
                //default:
                //    throw new UnexpectedLexemeException(TokenType.Integer, t);
        }
    }

    private bool IsComprassionOperator(TokenType type)
    {
        return type switch
        {
            TokenType.Equal or
            TokenType.LogicalNotEqual or
            TokenType.LessThan or
            TokenType.LessThanOrEqual or
            TokenType.GreaterThan or
            TokenType.GreaterThanOrEqual => true,
            _ => false,
        };
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
