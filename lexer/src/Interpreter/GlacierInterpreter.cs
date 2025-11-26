using Execution;

namespace Interpreter;
public class GlacierInterpreter
{
    private readonly Context context;
    private readonly IEnvironment environment;

    public GlacierInterpreter()
    {
        context = new Context();
        environment = new ConsoleEnvironment();
    }

    /// <summary>
    /// Выполняет программу на языке Glacier.
    /// </summary>
    /// <param name="sourceCode">Исходный код программы.</param>
    public void Execute(string sourceCode)
    {
        if (string.IsNullOrEmpty(sourceCode))
        {
            throw new ArgumentException("Source code cannot be null or empty", nameof(sourceCode));
        }

        // Создаем парсер и выполняем программу
        Parser.Parser parser = new(context, environment, sourceCode);
        parser.ParseProgram();
    }
}
