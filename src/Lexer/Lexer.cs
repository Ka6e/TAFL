namespace Lexer;
public class Lexer
{
    private static readonly Dictionary<string, TokenType> Keywords = new()
    {
        { "module", TokenType.Module },
        { "import", TokenType.Import },
        { "class", TokenType.Class },
        { "new", TokenType.New },
        { "func", TokenType.Func },
        { "let", TokenType.Let },
        { "var", TokenType.Var },
        { "enum", TokenType.Enum },
        { "interface", TokenType.Interface },
        { "if", TokenType.If },
        { "then", TokenType.Then },
        { "else", TokenType.Else },
        { "match", TokenType.Match },
        { "case", TokenType.Case },
        { "for", TokenType.For },
        { "while", TokenType.While },
        { "do", TokenType.Do },
        { "in", TokenType.In },
        { "break", TokenType.Break },
        { "continue", TokenType.Continue },
        { "return", TokenType.Return },
        { "throw", TokenType.Throw },
        { "try", TokenType.Try },
        { "catch", TokenType.Catch },
        { "type", TokenType.Type },
        { "where", TokenType.Where },
        { "implements", TokenType.Implements },
        { "this", TokenType.This },
        { "true", TokenType.True },
        { "false", TokenType.False },
        { "null", TokenType.NullLiteral },
        { "int", TokenType.IntegerType },
        { "char", TokenType.CharType },
        { "string", TokenType.StringType },
        { "float", TokenType.FloatType },
        { "void", TokenType.VoidType },
        { "bool", TokenType.BooleanType },
    };

    private readonly TextScanner scanner;

    public Lexer(string str)
    {
        scanner = new TextScanner(str);
    }

    public Token ParseToken()
    {
        SkipWhiteSpacesAndComments();

        if (scanner.IsEnd())
        {
            return new Token(TokenType.EndOfFile);
        }

        char c = scanner.Peek();

        if (char.IsLetter(c) || c == '_')
        {
            return ParseIdentifierOrKeyword();
        }

        if (char.IsAsciiDigit(c))
        {
            return ParseNumericLiteral();
        }

        if (c == '"' && scanner.Peek(1) == '"' && scanner.Peek(2) == '"')
        {
            return PasreMultiLineStringLiteral();
        }

        if (c == '"')
        {
            return ParseStringLiteral();
        }

        if (c == '`')
        {
            return ParseRawStringLiteral();
        }

        switch (c)
        {
            case '{':
                scanner.Advance();
                return new Token(TokenType.LBrace);
            case '}':
                scanner.Advance();
                return new Token(TokenType.RBrace);
            case '(':
                scanner.Advance();
                return new Token(TokenType.LParenthesis);
            case ')':
                scanner.Advance();
                return new Token(TokenType.RParenthesis);
            case '[':
                scanner.Advance();
                return new Token(TokenType.LSquareBracket);
            case ']':
                scanner.Advance();
                return new Token(TokenType.RSquareBracket);
            case '<':
                return ParseLessSign();
            case '>':
                return ParseGreaterSign();
            case '+':
                return ParsePlusOConcatinate();
            case '-':
                return ParseMinusSign();
            case '*':
                return ParseMultiply();
            case '/':
                return ParseDivide();
            case '%':
                scanner.Advance();
                return new Token(TokenType.ModSign);
            case '=':
                return ParseEqualSign();
            case '|':
                return ParseOrOperator();
            case '&':
                return ParseAndOperator();
            case '~':
                scanner.Advance();
                return new Token(TokenType.BitwiseNot);
            case '^':
                scanner.Advance();
                return new Token(TokenType.BitwiseXOR);
            case '!':
                return ParseLogicalNot();
            case '.':
                return ParseDot();
            case ':':
                return ParseDoubleDot();
            case ';':
                scanner.Advance();
                return new Token(TokenType.Semicolon);
            case ',':
                scanner.Advance();
                return new Token(TokenType.Comma);
        }

        scanner.Advance();
        return new Token(TokenType.Error, new TokenValue(c.ToString()));
    }

    private Token ParseIdentifierOrKeyword()
    {
        string value = scanner.Peek().ToString();
        scanner.Advance();

        for (char c = scanner.Peek(); char.IsLetter(c) || c == '_' || char.IsAsciiDigit(c); c = scanner.Peek())
        {
            value += c;
            scanner.Advance();
        }

        if (Keywords.TryGetValue(value, out TokenType type))
        {
            return new Token(type);
        }

        return new Token(TokenType.Identifier, new TokenValue(value));
    }

    private Token ParseNumericLiteral()
    {
        decimal value = GetDigitValue(scanner.Peek());
        scanner.Advance();

        for (char c = scanner.Peek(); char.IsAsciiDigit(c); c = scanner.Peek())
        {
            value = value * 10 + GetDigitValue(c);
            scanner.Advance();
        }

        if (scanner.Peek() == '.')
        {
            scanner.Advance();
            return ParseDecimalLiteral(value);
        }

        if (scanner.Peek() == 'x')
        {
            scanner.Advance();
            return ParseHexValue();
        }

        if (scanner.Peek() == 'b')
        {
            scanner.Advance();
            return ParseBinaryValue();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token ParseDecimalLiteral(decimal intPart)
    {
        decimal value = intPart;
        decimal factor = 0.1m;

        for (char c = scanner.Peek(); char.IsAsciiDigit(c); c = scanner.Peek())
        {
            scanner.Advance();
            value += factor * GetDigitValue(c);
            factor *= 0.1m;
        }

        return new Token(TokenType.Float, new TokenValue(value));
    }

    private Token ParseHexValue()
    {
        decimal value = 0;

        for (char c = scanner.Peek(); char.IsAsciiHexDigit(c); c = scanner.Peek())
        {
            value = value * 16 + GetHexDigitValue(c);
            scanner.Advance();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token ParseBinaryValue()
    {
        decimal value = 0;

        for (char c = scanner.Peek(); char.IsAsciiDigit(c); c = scanner.Peek())
        {
            value = value * 2 + GetDigitValue(c);
            scanner.Advance();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token PasreMultiLineStringLiteral()
    {
        scanner.Advance();
        scanner.Advance();
        scanner.Advance();

        string contents = "";
        while (!scanner.IsEnd())
        {
            if (scanner.Peek() == '"' && scanner.Peek(1) == '"' && scanner.Peek(2) == '"')
            {
                scanner.Advance();
                scanner.Advance();
                scanner.Advance();
                return new Token(TokenType.StringLiteral, new TokenValue(contents));
            }

            contents += scanner.Peek();
            scanner.Advance();
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private Token ParseRawStringLiteral()
    {
        scanner.Advance();

        string contents = "";

        while (!scanner.IsEnd())
        {
            char c = scanner.Peek();

            if (c == '`')
            {
                scanner.Advance();
                return new Token(TokenType.StringLiteral, new TokenValue(contents));
            }

            contents += c;
            scanner.Advance();
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private Token ParseStringLiteral()
    {
        scanner.Advance();

        string contents = "";

        while (scanner.Peek() != '"' && !scanner.IsEnd())
        {
            if (TryParseStringLiteralEscapeSequence(out char unescaped))
            {
                contents += unescaped;
            }
            else
            {
                contents += scanner.Peek();
                scanner.Advance();
            }
        }

        if (scanner.Peek() == '"')
        {
            scanner.Advance();
            return new Token(TokenType.StringLiteral, new TokenValue(contents));
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private bool TryParseStringLiteralEscapeSequence(out char unescaped)
    {
        if (scanner.Peek() != '\\')
        {
            unescaped = '\0';
            return false;
        }

        scanner.Advance();

        unescaped = scanner.Peek() switch
        {
            '\\' => '\\',
            '\'' => '\'',
            '\"' => '\"',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            _ => '\0'
        };

        if (unescaped != '\0')
        {
            scanner.Advance();
            return true;
        }

        return false;
    }

    private void SkipWhiteSpacesAndComments()
    {
        do
        {
            SkipWhiteSpace();
        }
        while (TryParseMultilineComment() || TryParseSingleLineComment());
    }

    private void SkipWhiteSpace()
    {
        while (char.IsWhiteSpace(scanner.Peek()))
        {
            scanner.Advance();
        }
    }

    private bool TryParseMultilineComment()
    {
        if (scanner.Peek() == '/' && scanner.Peek(1) == '*')
        {
            do
            {
                scanner.Advance();
            }
            while (!(scanner.Peek() == '*' && scanner.Peek(1) == '/'));

            scanner.Advance();
            scanner.Advance();
            return true;
        }

        return false;
    }

    private bool TryParseSingleLineComment()
    {
        if (scanner.Peek() == '/' && scanner.Peek(1) == '/')
        {
            do
            {
                scanner.Advance();
            }
            while (!scanner.IsEnd() && scanner.Peek() != '\n' && scanner.Peek() != '\r');

            return true;
        }

        return false;
    }

    private Token ParseOrOperator()
    {
        scanner.Advance();
        if (scanner.Peek() == '|')
        {
            scanner.Advance();
            return new Token(TokenType.LogicalOr);
        }

        return new Token(TokenType.BitwiseOr);
    }

    private Token ParseAndOperator()
    {
        scanner.Advance();
        if (scanner.Peek() == '&')
        {
            scanner.Advance();
            return new Token(TokenType.LogicalAnd);
        }

        return new Token(TokenType.BitwiseAnd);
    }

    private Token ParseLogicalNot()
    {
        scanner.Advance();
        if (scanner.Peek() == '=')
        {
            scanner.Advance();
            return new Token(TokenType.LogicalNotEqual);
        }

        return new Token(TokenType.LogicalNot);
    }

    private Token ParseEqualSign()
    {
        scanner.Advance();
        if (scanner.Peek() == '=')
        {
            scanner.Advance();
            return new Token(TokenType.Equal);
        }

        return new Token(TokenType.Assign);
    }

    private Token ParseLessSign()
    {
        scanner.Advance();

        char c = scanner.Peek();
        if (c == '=')
        {
            scanner.Advance();
            return new Token(TokenType.LessThanOrEqual);
        }

        if (char.IsLetter(c))
        {
            scanner.Advance();
            return new Token(TokenType.LessGeneric);
        }

        return new Token(TokenType.LessThan);
    }

    private Token ParseGreaterSign()
    {
        scanner.Advance();
        if (scanner.Peek() == '=')
        {
            scanner.Advance();
            return new Token(TokenType.GreaterThanOrEqual);
        }

        return new Token(TokenType.GreaterThan);
    }

    private Token ParseDivide()
    {
        scanner.Advance();
        if (scanner.Peek() == '/')
        {
            scanner.Advance();
            return new Token(TokenType.IntegerDivide);
        }

        return new Token(TokenType.DivideSign);
    }

    private Token ParseMultiply()
    {
        scanner.Advance();
        if (scanner.Peek() == '*')
        {
            scanner.Advance();
            return new Token(TokenType.Exponent);
        }

        return new Token(TokenType.MultiplySign);
    }

    private Token ParsePlusOConcatinate()
    {
        scanner.Advance();
        if (scanner.Peek() == '+')
        {
            scanner.Advance();
            return new Token(TokenType.Increment);
        }

        return new Token(TokenType.PlusSign);
    }

    private Token ParseMinusSign()
    {
        scanner.Advance();

        if (scanner.Peek() == '-')
        {
            scanner.Advance();
            return new Token(TokenType.Dicrement);
        }

        return new Token(TokenType.MinusSign);
    }

    private Token ParseDot()
    {
        if (scanner.Peek(1) == '.')
        {
            scanner.Advance();
            scanner.Advance();
            return new Token(TokenType.Range);
        }

        scanner.Advance();
        return new Token(TokenType.Access);
    }

    private int GetHexDigitValue(char c)
    {
        if (char.IsDigit(c))
        {
            return c - '0';
        }
        else if (c >= 'a' && c <= 'f')
        {
            return 10 + (c - 'a');
        }
        else
        {
            return 10 + (c - 'A');
        }
    }

    private int GetDigitValue(char c)
    {
        return c - '0';
    }

    private Token ParseDoubleDot()
    {
        scanner.Advance();
        if (scanner.Peek() == '=')
        {
            scanner.Advance();
            return new Token(TokenType.Assign);
        }

        return new Token(TokenType.Annotation);
    }
}
