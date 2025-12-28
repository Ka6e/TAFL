using Ast;
using Ast.Declarations;
using Ast.Expressions;
using Ast.Programm;
using Ast.Statement;

using Lexer;

using ValueType = Runtime.ValueType;

namespace Parser;
public class Parser
{
    private readonly TokenStream tokens;
    private readonly Stack<ValueType> returnTypes = new();

    public Parser(string code)
    {
        tokens = new TokenStream(code);
    }

    /// <summary>
    /// program = module_decl, { import_decl }, { top_level_decl } ;.
    /// </summary>
    public ProgramNode ParseProgram()
    {
        ModuleDecl module;
        List<ImportDecl> imports = new();
        List<AstNode> topLevel = new();

        if (tokens.Peek().Type != TokenType.Module)
        {
            throw new UnexpectedLexemeException(TokenType.Module, tokens.Peek());
        }

        module = ParseModuleDecl();

        while (tokens.Peek().Type == TokenType.Import)
        {
            imports.Add(ParseImportDecl());
        }

        while (tokens.Peek().Type != TokenType.EndOfFile)
        {
            topLevel.Add(ParseTopLevelDecl());
        }

        return new ProgramNode(module, imports, topLevel);
    }

    /// <summary>
    /// module_decl = "module", identifier ;.
    /// </summary>
    private ModuleDecl ParseModuleDecl()
    {
        Match(TokenType.Module);
        string name = Match(TokenType.Identifier).Value!.ToString();

        return new ModuleDecl(name);
    }

    /// <summary>
    /// import_decl = "import", identifier ;.
    /// </summary>
    private ImportDecl ParseImportDecl()
    {
        Match(TokenType.Import);
        string name = Match(TokenType.Identifier).Value!.ToString();

        return new ImportDecl(name);
    }

    /// <summary>
    /// top_level_decl = class_decl
    ///                | enum_decl
    ///                | function_decl
    ///                | statement ;.
    /// </summary>
    private AstNode ParseTopLevelDecl()
    {
        return tokens.Peek().Type switch
        {
            TokenType.Class => throw new NotImplementedException(),
            TokenType.Enum => throw new NotImplementedException(),
            TokenType.Func => ParseFuncDecl(),
            _ => ParseStatement(),
        };
    }

    /// <summary>
    /// function_decl = "func", identifier, "(", [ parameter_list ], ")", [ ":", type_annotation ], block ;.
    /// </summary>
    private FunctionDecl ParseFuncDecl()
    {
        Match(TokenType.Func);

        string name = Match(TokenType.Identifier).Value!.ToString();

        Match(TokenType.LParenthesis);
        List<Parameter> parametrs = new List<Parameter>();

        if (tokens.Peek().Type != TokenType.RParenthesis)
        {
            parametrs = ParseParameterList();
        }

        Match(TokenType.RParenthesis);

        ValueType returnType = ValueType.Void;
        if (tokens.Peek().Type == TokenType.Annotation)
        {
            tokens.Advance();
            returnType = ParseTypeAnnotation();
            tokens.Advance();
        }

        returnTypes.Push(returnType);

        BlockStatement body = ParseBlock();

        returnTypes.Pop();

        return new FunctionDecl(name, parametrs, returnType, body);
    }

    /// <summary>
    /// parameter_list = parameter, { ",", parameter } ;.
    /// </summary>
    private List<Parameter> ParseParameterList()
    {
        List<Parameter> parameters = new()
        {
            ParseParameter(),
        };

        while (tokens.Peek().Type == TokenType.Comma)
        {
            tokens.Advance();
            parameters.Add(ParseParameter());
        }

        return parameters;
    }

    /// <summary>
    /// parameter = identifier, [ ":", type_annotation ] ;.
    /// </summary>
    private Parameter ParseParameter()
    {
        string name = Match(TokenType.Identifier).Value!.ToString();

        ValueType type;

        if (tokens.Peek().Type == TokenType.Annotation)
        {
            tokens.Advance();
            type = ParseTypeAnnotation();
            tokens.Advance();
        }
        else
        {
            throw new ArgumentException($"Parameter '{name}' without type");
        }

        return new Parameter(name, type);
    }

    /// <summary>
    /// statement = variable_decl
    ///      | if_statement
    ///      | for_statement
    ///      | while_statement
    ///      | do_while_statement
    ///      | break_statement
    ///      | continue_statement
    ///      | return_statement
    ///      | match_statement
    ///      | expression_statement ;.
    /// </summary>
    private Statement ParseStatement()
    {
        Token t = tokens.Peek();
        return t.Type switch
        {
            TokenType.Var => ParseVariableDecl(),
            TokenType.Let => ParseVariableDecl(),
            TokenType.If => ParseIfStatement(),
            TokenType.For => ParseForStatement(),
            TokenType.While => ParseWhileStatement(),
            TokenType.Do => ParseDoWhileStatement(),
            TokenType.Break => ParseBreakStatement(),
            TokenType.Continue => ParseContinueStatement(),
            TokenType.Return => ParseReturnStatement(),
            _ => ParseExpressionStatement(),
        };
    }

    /// <summary>
    /// expression_statement = expression, ";" ;.
    /// </summary>
    private ExpressionStatement ParseExpressionStatement()
    {
        Expression value = ParseExpression();
        Match(TokenType.Semicolon);

        return new ExpressionStatement(value);
    }

    /// <summary>
    /// variable_decl = ("let" | "var"), identifier, [ ":", type_annotation ], [ "=", expression ], ";" ;.
    /// </summary>
    private VariableDeclarationStatement ParseVariableDecl()
    {
        if (tokens.Peek().Type == TokenType.Var)
        {
            Match(TokenType.Var);
            string varName = Match(TokenType.Identifier).Value!.ToString();
            if (tokens.Peek().Type != TokenType.Assign)
            {
                throw new UnexpectedLexemeException(TokenType.Assign, tokens.Peek());
            }

            Match(TokenType.Assign);
            Expression varValue = ParseExpression();
            Match(TokenType.Semicolon);

            return new VariableDeclarationStatement(varName, null, varValue);
        }

        Match(TokenType.Let);
        string name = Match(TokenType.Identifier).Value!.ToString();
        Match(TokenType.Annotation);
        ValueType type = ParseTypeAnnotation();
        tokens.Advance();
        if (tokens.Peek().Type != TokenType.Assign)
        {
            Match(TokenType.Semicolon);
            return new VariableDeclarationStatement(name, type, null);
        }

        Match(TokenType.Assign);
        Expression value = ParseExpression();
        Match(TokenType.Semicolon);

        return new VariableDeclarationStatement(name, type, value);
    }

    /// <summary>
    /// break_statement = "break", ";" ;.
    /// </summary>
    private BreakStatement ParseBreakStatement()
    {
        Match(TokenType.Break);
        Match(TokenType.Semicolon);

        return new BreakStatement();
    }

    /// <summary>
    /// continue_statement = "continue", ";" ;.
    /// </summary>
    private ContinueStatement ParseContinueStatement()
    {
        Match(TokenType.Continue);
        Match(TokenType.Semicolon);

        return new ContinueStatement();
    }

    /// <summary>
    /// return_statement = "return", [ expression ], ";" ;.
    /// </summary>
    private ReturnStatement ParseReturnStatement()
    {
        Match(TokenType.Return);
        if (tokens.Peek().Type == TokenType.Semicolon)
        {
            Match(TokenType.Semicolon);
            return new ReturnStatement(null, ValueType.Void);
        }

        Expression value = ParseExpression();
        Match(TokenType.Semicolon);

        return new ReturnStatement(value, returnTypes.Peek());
    }

    /// <summary>
    /// for_statement = "for", assignment_or_empty, expression, ",", assignment_or_empty, "in", block ;.
    /// </summary>
    private ForLoopStatement ParseForStatement()
    {
        Match(TokenType.For);

        VariableDeclarationStatement? init = null;

        if (tokens.Peek().Type != TokenType.Comma)
        {
            init = ParseForInit();
        }

        Match(TokenType.Comma);
        Expression? condition = null;
        if (tokens.Peek().Type != TokenType.Comma)
        {
            condition = ParseExpression();
        }

        Match(TokenType.Comma);

        AssignmentExpression? step = null;
        if (tokens.Peek().Type != TokenType.In)
        {
            step = ParseForStep();
        }

        Match(TokenType.In);

        BlockStatement block = ParseBlock();

        return new ForLoopStatement(init, condition, step, block);
    }

    /// <summary>
    /// if_statement = "if", expression, "then", block, [ "else", block ] ;.
    /// </summary>
    private IfElseStatement ParseIfStatement()
    {
        Match(TokenType.If);
        Expression condition = ParseExpression();
        Match(TokenType.Then);
        BlockStatement thenBlock = ParseBlock();
        BlockStatement? elseBlock = null;

        if (tokens.Peek().Type == TokenType.Else)
        {
            tokens.Advance();
            elseBlock = ParseBlock();
        }

        return new IfElseStatement(condition, thenBlock, elseBlock);
    }

    /// <summary>
    /// while_statement = "while", "(", expression, ")", block ;.
    /// </summary>
    private WhileLoopStatement ParseWhileStatement()
    {
        Match(TokenType.While);
        Match(TokenType.LParenthesis);
        Expression condiiton = ParseExpression();
        Match(TokenType.RParenthesis);
        BlockStatement block = ParseBlock();

        return new WhileLoopStatement(condiiton, block);
    }

    /// <summary>
    /// do_while_statement = "do", block, "while", "(", expression, ")", ";" ;.
    /// </summary>
    private DoWhileLoopStatement ParseDoWhileStatement()
    {
        Match(TokenType.Do);
        BlockStatement block = ParseBlock();
        Match(TokenType.While);
        Match(TokenType.LParenthesis);
        Expression condition = ParseExpression();
        Match(TokenType.RParenthesis);
        Match(TokenType.Semicolon);

        return new DoWhileLoopStatement(block, condition);
    }

    /// <summary>
    /// block = "{", { statement }, "}" ;.
    /// </summary>
    private BlockStatement ParseBlock()
    {
        Match(TokenType.LBrace);
        List<Statement> statements = new List<Statement>();

        while (tokens.Peek().Type != TokenType.RBrace)
        {
            statements.Add(ParseStatement());
        }

        Match(TokenType.RBrace);

        return new BlockStatement(statements);
    }

    /// <summary>
    /// expressions_list = expression, { ",", expression } ;.
    /// </summary>
    private List<Expression> ParseExpressionList()
    {
        List<Expression> values = new()
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
    /// expression = assignment_expression ;.
    /// </summary>
    private Expression ParseExpression()
    {
        return ParseAssigmnetExpression();
    }

    /// <summary>
    /// assignment_expression = logical_or_expression, [ "=",  assignment_expression ];.
    /// </summary>
    private Expression ParseAssigmnetExpression()
    {
        Expression left = ParseLogicalOrExpression();

        if (tokens.Peek().Type == TokenType.Assign)
        {
            if (left is not VariableExpression v)
            {
                throw new Exception("Left side of assignment must be a variable");
            }

            tokens.Advance();
            Expression right = ParseAssigmnetExpression();

            return new AssignmentExpression(v.Name, right);
        }

        return left;
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
                    Expression multiplyRight = ParseUnaryExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Multiply, multiplyRight);
                    break;
                case TokenType.DivideSign:
                    tokens.Advance();
                    Expression devideRight = ParseUnaryExpression();
                    left = new BinaryOperationExpression(left, BinaryOperation.Divide, devideRight);
                    break;
                case TokenType.ModSign:
                    tokens.Advance();
                    Expression moduleRight = ParseUnaryExpression();
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
                tokens.Advance();
                return new LiteralExpression(new Runtime.Value((int)t.Value!.ToDecimal()));
            case TokenType.Float:
                tokens.Advance();
                return new LiteralExpression(new Runtime.Value(t.Value!.ToDecimal()));
            case TokenType.True:
                tokens.Advance();
                return new LiteralExpression(new Runtime.Value(true));
            case TokenType.False:
                tokens.Advance();
                return new LiteralExpression(new Runtime.Value(false));
            case TokenType.StringLiteral:
                tokens.Advance();
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

    private Expression ParseFunctionCall(string name)
    {
        Match(TokenType.LParenthesis);
        List<Expression> arguments = new List<Expression>();

        if (tokens.Peek().Type != TokenType.RParenthesis)
        {
            arguments = ParseExpressionList();
        }

        Match(TokenType.RParenthesis);

        return new FunctionCallExpression(name, arguments);
    }

    private VariableDeclarationStatement ParseForInit()
    {
        string name = Match(TokenType.Identifier).Value!.ToString();
        Match(TokenType.Assign);
        Expression value = ParseExpression();

        return new VariableDeclarationStatement(name, null, value);
    }

    private AssignmentExpression ParseForStep()
    {
        Expression expr = ParseAssigmnetExpression();

        if (expr is not AssignmentExpression assignment)
        {
            throw new Exception("For step must be assignment");
        }

        return assignment;
    }

    private ValueType ParseTypeAnnotation()
    {
        Token t = tokens.Peek();

        return t.Type switch
        {
            TokenType.IntegerType => ValueType.Int,
            TokenType.StringType => ValueType.String,
            TokenType.BooleanType => ValueType.Bool,
            TokenType.FloatType => ValueType.Float,
            _ => throw new UnexpectedLexemeException(t)
        };
    }
}
