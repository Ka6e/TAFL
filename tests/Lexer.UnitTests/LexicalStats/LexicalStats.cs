using System.Text;

namespace Lexer.UnitTests.LexicalStats;
public static class LexicalStats
{
    private static readonly string[] OutputOrder =
    {
            "keywords",
            "identifier",
            "number literals",
            "string literals",
            "operators",
            "other lexemes",
    };

    private static readonly HashSet<TokenType> KeyWords = new()
    {
        TokenType.Module, TokenType.Import, TokenType.New,
        TokenType.Func, TokenType.Let, TokenType.Var, TokenType.Enum,
        TokenType.Interface, TokenType.If, TokenType.Then, TokenType.Else,
        TokenType.For, TokenType.Return, TokenType.Throw,
        TokenType.Try, TokenType.Catch, TokenType.Type, TokenType.Where,
        TokenType.Implements, TokenType.This,
        TokenType.True, TokenType.False, TokenType.NullLiteral,
        TokenType.IntegerType, TokenType.FloatType, TokenType.StringType, TokenType.CharType,
        TokenType.BooleanType, TokenType.VoidType, TokenType.In,
    };

    private static readonly HashSet<TokenType> Operators = new()
    {
        TokenType.PlusSign, TokenType.MinusSign, TokenType.MultiplySign,
        TokenType.DivideSign, TokenType.Exponent, TokenType.IntegerDivide,
        TokenType.ModSign, TokenType.Assign, TokenType.Equal,
        TokenType.LogicalOr, TokenType.BitwiseOr, TokenType.LogicalAnd, TokenType.BitwiseAnd,
        TokenType.LogicalNot, TokenType.LogicalNotEqual, TokenType.BitwiseNot, TokenType.BitwiseXOR,
        TokenType.Range, TokenType.Annotation,
        TokenType.Increment, TokenType.Dicrement,
        TokenType.LessThan, TokenType.LessThanOrEqual,
        TokenType.GreaterThan, TokenType.GreaterThanOrEqual, TokenType.Access,
    };

    public static string CollectFromFile(string path)
    {
        string code = File.ReadAllText(path);
        Lexer lexer = new Lexer(code);

        Dictionary<string, int> counts = new Dictionary<string, int>
        {
            ["keywords"] = 0,
            ["identifier"] = 0,
            ["number literals"] = 0,
            ["string literals"] = 0,
            ["operators"] = 0,
            ["other lexemes"] = 0,
        };

        Token token;
        while ((token = lexer.ParseToken()).Type != TokenType.EndOfFile)
        {
            CategorizeToken(token.Type, counts);
        }

        StringBuilder sb = new StringBuilder();
        foreach (string key in OutputOrder)
        {
            sb.Append(key).Append(": ").Append(counts[key]).AppendLine();
        }

        return sb.ToString().TrimEnd('\r', '\n');
    }

    private static void CategorizeToken(TokenType type, Dictionary<string, int> counts)
    {
        if (KeyWords.Contains(type))
        {
            counts["keywords"]++;
            return;
        }

        if (type == TokenType.Identifier)
        {
            counts["identifier"]++;
            return;
        }

        if (type == TokenType.Integer || type == TokenType.Float)
        {
            counts["number literals"]++;
            return;
        }

        if (type == TokenType.StringLiteral)
        {
            counts["string literals"]++;
            return;
        }

        if (Operators.Contains(type))
        {
            counts["operators"]++;
            return;
        }

        counts["other lexemes"]++;
    }
}
