using Parser;

namespace Interpreter;
public static class Program
{
    public static int Main(string[] args)
    {
        try
        {
            string sourceCode = string.Empty;
            string? line;
            while ((line = Console.ReadLine()) != null)
            {
                sourceCode += line;
            }

            GlacierInterpreter interpreter = new GlacierInterpreter(new ConsoleEnvironment());
            interpreter.Execute(sourceCode);

            return 0;
        }
        catch (UnexpectedLexemeException ex)
        {
            Console.Error.WriteLine($"Parse error: {ex.Message}");
            return 1;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Error: {ex.Message}");
            return 1;
        }
    }
}
