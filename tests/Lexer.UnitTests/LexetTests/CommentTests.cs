namespace Lexer.UnitTests.LexetTests;
public class CommentTests
{
    [Theory]
    [MemberData(nameof(GetSingleLineCommentData))]
    public void Can_tokenize_single_comment(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetSingleLineCommentData()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                """
                //Это комментарий
                let x = a // Объявление и присвоение
                """,
                [
                    new Token(TokenType.Let),
                    new Token(TokenType.Identifier, new TokenValue("x")),
                    new Token(TokenType.Assign),
                    new Token(TokenType.Identifier, new TokenValue("a"))
                ]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetMultiLineCommentData))]
    public void Can_tokenize_multi_line_commend(string code, List<Token> expected)
    {
        List<Token> actual = LexerTest.Tokenize(code);
        Assert.Equal(expected, actual);
    }

    public static TheoryData<string, List<Token>> GetMultiLineCommentData()
    {
        return new TheoryData<string, List<Token>>()
        {
            {
                """
                /*let x = 1;
                var a = x;*/
                module Main

                func calculator {
                let factor: int

                new(f: int) {
                    this.factor = f
                    }
                }
                """,
                [
                    new Token(TokenType.Module),
                    new Token(TokenType.Identifier, new TokenValue("Main")),
                    new Token(TokenType.Func),
                    new Token(TokenType.Identifier, new TokenValue("calculator")),
                    new Token(TokenType.LBrace),
                    new Token(TokenType.Let),
                    new Token(TokenType.Identifier, new TokenValue("factor")),
                    new Token(TokenType.Annotation),
                    new Token(TokenType.IntegerType),
                    new Token(TokenType.New),
                    new Token(TokenType.LParenthesis),
                    new Token(TokenType.Identifier, new TokenValue("f")),
                    new Token(TokenType.Annotation),
                    new Token(TokenType.IntegerType),
                    new Token(TokenType.RParenthesis),
                    new Token(TokenType.LBrace),
                    new Token(TokenType.This),
                    new Token(TokenType.Access),
                    new Token(TokenType.Identifier, new TokenValue("factor")),
                    new Token(TokenType.Assign),
                    new Token(TokenType.Identifier, new TokenValue("f")),
                    new Token(TokenType.RBrace),
                    new Token(TokenType.RBrace),
                ]
            },
        };
    }
}
