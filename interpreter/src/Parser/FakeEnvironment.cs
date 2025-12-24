using System.Globalization;

using Ast.Declarations;

using Execution;

using Runtime;

namespace Parser;

/// <summary>
/// Поддельное окружение: работает как настоящее, но не совершает реального ввода/вывода.
/// </summary>
public class FakeEnvironment : IEnvironment
{
    private readonly List<decimal> results = [];
    private readonly List<string> output = [];

    private readonly Dictionary<string, ModuleDecl> modules = [];
    private readonly Dictionary<string, ImportDecl> imports = [];

    public IReadOnlyList<decimal> Results => results;
    
    public IReadOnlyList<string> Output => output;

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
        results.Add(result);
    }

    public void AddResult(Value result)
    {
        throw new NotImplementedException();
    }

    public void Print(decimal result)
    {
        results.Add(result);
    }

    public void Print(string result)
    {
        output.Add(result);
    }

    public void PrintLine()
    {
        output.Add("");
    }

    public void PrintLine(string line)
    {
        output.Add(line);
    }

    public void PrintLine(decimal line)
    {
        results.Add(line);
    }

    public decimal ReadNumber()
    {
        return 0;
        //decimal.TryParse(Console.ReadLine() ?? "", out decimal result);

        //return result;
    }

    public void RemoveImport(ImportDecl import)
    {
        throw new NotImplementedException();
    }

    public void RemoveModule(ModuleDecl module)
    {
        throw new NotImplementedException();
    }

    public void WriteNumber(decimal result)
    {
        Console.WriteLine(result.ToString("0.#####", CultureInfo.InvariantCulture));
    }
}