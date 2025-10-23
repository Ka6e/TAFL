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
        { "for", TokenType.For },
        { "in", TokenType.In },
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
        { "None", TokenType.NullLiteral },
        { "int", TokenType.IntegerType },
        { "char", TokenType.CharType },
        { "string", TokenType.StringType },
        { "float", TokenType.FloatType },
        { "void", TokenType.VoidType },
        { "bool", TokenType.BooleanType },
    };

    private readonly TextScanner _scanner;

    public Lexer(string str)
    {
        _scanner = new TextScanner(str);
    }

    public Token ParseToken()
    {
        SkipWhiteSpacesAndComments();

        if (_scanner.IsEnd())
        {
            return new Token(TokenType.EndOfFile);
        }

        char c = _scanner.Peek();

        if (char.IsLetter(c) || c == '_')
        {
            return ParseIdentifierOrKeyword();
        }

        if (char.IsAsciiDigit(c))
        {
            return ParseNumericLiteral();
        }

        if (c == '"' && _scanner.Peek(1) == '"' && _scanner.Peek(2) == '"')
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
                _scanner.Advance();
                return new Token(TokenType.LBrace);
            case '}':
                _scanner.Advance();
                return new Token(TokenType.RBrace);
            case '(':
                _scanner.Advance();
                return new Token(TokenType.LParenthesis);
            case ')':
                _scanner.Advance();
                return new Token(TokenType.RParenthesis);
            case '[':
                _scanner.Advance();
                return new Token(TokenType.LSquareBracket);
            case ']':
                _scanner.Advance();
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
                _scanner.Advance();
                return new Token(TokenType.ModSign);
            case '=':
                return ParseEqualSign();
            case '|':
                return ParseOrOperator();
            case '&':
                return ParseAndOperator();
            case '~':
                _scanner.Advance();
                return new Token(TokenType.BitwiseNot);
            case '^':
                _scanner.Advance();
                return new Token(TokenType.BitwiseXOR);
            case '!':
                return ParseLogicalNot();
            case '.':
                return ParseDot();
            case ':':
                return ParseDoubleDot();
            case ';':
                _scanner.Advance();
                return new Token(TokenType.Semicolon);
            case ',':
                _scanner.Advance();
                return new Token(TokenType.Comma);
        }

        _scanner.Advance();
        return new Token(TokenType.Error, new TokenValue(c.ToString()));
    }

    private Token ParseIdentifierOrKeyword()
    {
        string value = _scanner.Peek().ToString();
        _scanner.Advance();

        for (char c = _scanner.Peek(); char.IsLetter(c) || c == '_' || char.IsAsciiDigit(c); c = _scanner.Peek())
        {
            value += c;
            _scanner.Advance();
        }

        if (Keywords.TryGetValue(value, out TokenType type))
        {
            return new Token(type);
        }

        return new Token(TokenType.Identifier, new TokenValue(value));
    }

    private Token ParseNumericLiteral()
    {
        decimal value = GetDigitValue(_scanner.Peek());
        _scanner.Advance();

        for (char c = _scanner.Peek(); char.IsAsciiDigit(c); c = _scanner.Peek())
        {
            value = value * 10 + GetDigitValue(c);
            _scanner.Advance();
        }

        if (_scanner.Peek() == '.')
        {
            _scanner.Advance();
            return ParseDecimalLiteral(value);
        }

        if (_scanner.Peek() == 'x')
        {
            _scanner.Advance();
            return ParseHexValue();
        }

        if (_scanner.Peek() == 'b')
        {
            _scanner.Advance();
            return ParseBinaryValue();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token ParseDecimalLiteral(decimal intPart)
    {
        decimal value = intPart;
        decimal factor = 0.1m;

        for (char c = _scanner.Peek(); char.IsAsciiDigit(c); c = _scanner.Peek())
        {
            _scanner.Advance();
            value += factor * GetDigitValue(c);
            factor *= 0.1m;
        }

        return new Token(TokenType.Float, new TokenValue(value));
    }

    private Token ParseHexValue()
    {
        decimal value = 0;

        for (char c = _scanner.Peek(); char.IsAsciiHexDigit(c); c = _scanner.Peek())
        {
            value = value * 16 + GetHexDigitValue(c);
            _scanner.Advance();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token ParseBinaryValue()
    {
        decimal value = 0;

        for (char c = _scanner.Peek(); char.IsAsciiDigit(c); c = _scanner.Peek())
        {
            value = value * 2 + GetDigitValue(c);
            _scanner.Advance();
        }

        return new Token(TokenType.Integer, new TokenValue(value));
    }

    private Token PasreMultiLineStringLiteral()
    {
        _scanner.Advance();
        _scanner.Advance();
        _scanner.Advance();

        string contents = "";
        while (!_scanner.IsEnd())
        {
            if (_scanner.Peek() == '"' && _scanner.Peek(1) == '"' && _scanner.Peek(2) == '"')
            {
                _scanner.Advance();
                _scanner.Advance();
                _scanner.Advance();
                return new Token(TokenType.StringLiteral, new TokenValue(contents));
            }

            contents += _scanner.Peek();
            _scanner.Advance();
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private Token ParseRawStringLiteral()
    {
        _scanner.Advance();

        string contents = "";

        while (!_scanner.IsEnd())
        {
            char c = _scanner.Peek();

            if (c == '`')
            {
                _scanner.Advance();
                return new Token(TokenType.StringLiteral, new TokenValue(contents));
            }

            contents += c;
            _scanner.Advance();
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private Token ParseStringLiteral()
    {
        _scanner.Advance();

        string contents = "";

        while (_scanner.Peek() != '"' && !_scanner.IsEnd())
        {
            if (TryParseStringLiteralEscapeSequence(out char unescaped))
            {
                contents += unescaped;
            }
            else
            {
                contents += _scanner.Peek();
                _scanner.Advance();
            }
        }

        if (_scanner.Peek() == '"')
        {
            _scanner.Advance();
            return new Token(TokenType.StringLiteral, new TokenValue(contents));
        }

        return new Token(TokenType.Error, new TokenValue(contents));
    }

    private bool TryParseStringLiteralEscapeSequence(out char unescaped)
    {
        if (_scanner.Peek() != '\\')
        {
            unescaped = '\0';
            return false;
        }

        _scanner.Advance();

        unescaped = _scanner.Peek() switch
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
            _scanner.Advance();
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
        while (char.IsWhiteSpace(_scanner.Peek()))
        {
            _scanner.Advance();
        }
    }

    private bool TryParseMultilineComment()
    {
        if (_scanner.Peek() == '/' && _scanner.Peek(1) == '*')
        {
            do
            {
                _scanner.Advance();
            }
            while (!(_scanner.Peek() == '*' && _scanner.Peek(1) == '/'));

            _scanner.Advance();
            _scanner.Advance();
            return true;
        }

        return false;
    }

    private bool TryParseSingleLineComment()
    {
        if (_scanner.Peek() == '/' && _scanner.Peek(1) == '/')
        {
            do
            {
                _scanner.Advance();
            }
            while (!_scanner.IsEnd() && _scanner.Peek() != '\n' && _scanner.Peek() != '\r');

            return true;
        }

        return false;
    }

    private Token ParseOrOperator()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '|')
        {
            _scanner.Advance();
            return new Token(TokenType.LogicalOr);
        }

        return new Token(TokenType.BitwiseOr);
    }

    private Token ParseAndOperator()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '&')
        {
            _scanner.Advance();
            return new Token(TokenType.LogicalAnd);
        }

        return new Token(TokenType.BitwiseAnd);
    }

    private Token ParseLogicalNot()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '=')
        {
            _scanner.Advance();
            return new Token(TokenType.LogicalNotEqual);
        }

        return new Token(TokenType.LogicalNot);
    }

    private Token ParseEqualSign()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '=')
        {
            _scanner.Advance();
            return new Token(TokenType.Equal);
        }

        return new Token(TokenType.Assign);
    }

    private Token ParseLessSign()
    {
        _scanner.Advance();

        char c = _scanner.Peek();
        if (c == '=')
        {
            _scanner.Advance();
            return new Token(TokenType.LessThanOrEqual);
        }

        if (char.IsLetter(c))
        {
            _scanner.Advance();
            return new Token(TokenType.LessGeneric);
        }

        return new Token(TokenType.LessThan);
    }

    private Token ParseGreaterSign()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '=')
        {
            _scanner.Advance();
            return new Token(TokenType.GreaterThanOrEqual);
        }

        return new Token(TokenType.GreaterThan);
    }

    private Token ParseDivide()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '/')
        {
            _scanner.Advance();
            return new Token(TokenType.IntegerDivide);
        }

        return new Token(TokenType.DivideSign);
    }

    private Token ParseMultiply()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '*')
        {
            _scanner.Advance();
            return new Token(TokenType.Exponent);
        }

        return new Token(TokenType.MultiplySign);
    }

    private Token ParsePlusOConcatinate()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '+')
        {
            _scanner.Advance();
            return new Token(TokenType.Increment);
        }

        return new Token(TokenType.PlusSign);
    }

    private Token ParseMinusSign()
    {
        _scanner.Advance();
        if (_scanner.Peek() == '>')
        {
            _scanner.Advance();
            return new Token(TokenType.FuncType);
        }

        if (_scanner.Peek() == '-')
        {
            _scanner.Advance();
            return new Token(TokenType.Dicrement);
        }

        return new Token(TokenType.MinusSign);
    }

    private Token ParseDot()
    {
        if (_scanner.Peek(1) == '.')
        {
            _scanner.Advance();
            _scanner.Advance();
            return new Token(TokenType.Range);
        }

        _scanner.Advance();
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
        _scanner.Advance();
        if (_scanner.Peek() == '=')
        {
            _scanner.Advance();
            return new Token(TokenType.Assign);
        }

        return new Token(TokenType.Annotation);
    }
}
