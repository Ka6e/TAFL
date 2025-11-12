using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

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
            // Создаём лексический анализатор
            AntlrInputStream inputStream = new(expression);
            GlacierLexer lexer = new(inputStream);

            // Создаём синтаксический анализатор
            CommonTokenStream tokenStream = new(lexer);
            GlacierParser parser = new(tokenStream);

            // Устанавливаем обработчик ошибок
            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowingErrorListener());

            // Отключаем механизм восстановления после ошибок
            parser.ErrorHandler = new BailErrorStrategy();

            // Запускаем разбор по правилу expressionRoot
            parser.expressionRoot();
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