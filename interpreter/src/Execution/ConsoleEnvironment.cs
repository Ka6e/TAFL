using System.Globalization;

using Ast.Declarations;

using Runtime;

namespace Execution;
public class ConsoleEnvironment : IEnvironment
{
    private readonly Queue<string> inputQueue = new();
    private readonly Dictionary<string, ModuleDecl> modules = new();
    private readonly Dictionary<string, ImportDecl> imports = new();

    public List<decimal> Results { get; } = new();

    public string Output { get; private set; } = string.Empty;

    public void AddImport(ImportDecl import)
    {
        if (!imports.ContainsKey(import.Name))
        {
            imports.Add(import.Name, import);
        }
    }

    public void AddModule(ModuleDecl module)
    {
        if (!modules.ContainsKey(module.Name))
        {
            modules.Add(module.Name, module);
        }
    }

    public void AddResult(decimal result)
    {
        Results.Add(result);
    }

    public void AddResult(Value result)
    {
        throw new NotImplementedException();
    }

    public void Print(decimal result)
    {
        string formatted = (result % 1 == 0)
            ? result.ToString("0", CultureInfo.InvariantCulture)
            : result.ToString("0.#####", CultureInfo.InvariantCulture);

        Output += formatted;
    }

    public void Print(string result)
    {
        Output += result;
    }

    public void PrintLine(string line)
    {
        Print(line);
        PrintLine();
    }

    public void PrintLine(decimal line)
    {
        Print(line);
        PrintLine();
    }

    public void PrintLine()
    {
        Output += Environment.NewLine;
        Console.WriteLine();
    }

    public string ReadInput()
    {
        if (inputQueue.Count == 0)
        {
            throw new InvalidOperationException("No input values available.");
        }

        return inputQueue.Dequeue().ToString(CultureInfo.InvariantCulture);
    }

    public decimal ReadNumber()
    {
        string input = ReadInput();
        decimal result = decimal.Parse(input, CultureInfo.InvariantCulture);
        return result;
    }

    public void RemoveImport(ImportDecl import)
    {
        imports.Remove(import.Name);
    }

    public void RemoveModule(ModuleDecl module)
    {
        modules.Remove(module.Name);
    }
}