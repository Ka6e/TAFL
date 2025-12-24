using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System.IO;

namespace GlacierGrammar
{
    public static class GlacierGrammar
    {
        /// <summary>
        /// Проверяет выражение на соответствие грамматике Glacier.
        /// Бросает исключение при несоответствии.
        /// </summary>
        public static void ValidateExpression(string expression)
        {
            AntlrInputStream inputStream = new(expression);
            GlacierLexer lexer = new(inputStream);
            CommonTokenStream tokenStream = new(lexer);
            GlacierParser parser = new(tokenStream);

            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowingErrorListener());
            parser.ErrorHandler = new BailErrorStrategy();

            parser.expressionRoot();
        }

        /// <summary>
        /// Проверяет программу на соответствие грамматике Glacier.
        /// Бросает исключение при несоответствии.
        /// </summary>
        public static void ValidateProgram(string code)
        {
            AntlrInputStream inputStream = new(code);
            GlacierLexer lexer = new(inputStream);
            CommonTokenStream tokenStream = new(lexer);
            GlacierParser parser = new(tokenStream);

            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowingErrorListener());
            parser.ErrorHandler = new BailErrorStrategy();

            parser.program();
        }
    }

    // Обработчик ошибок, который бросает исключение при синтаксической ошибке
    public class ThrowingErrorListener : BaseErrorListener
    {
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol,
            int line, int charPositionInLine, string msg, RecognitionException e)
        {
            throw new ParseCanceledException($"Синтаксическая ошибка в позиции {line}:{charPositionInLine} - {msg}");
        }
    }
}
