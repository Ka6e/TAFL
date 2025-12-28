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
    private readonly List<Value> results = [];
    private readonly List<string> output = [];

    private readonly Dictionary<string, ModuleDecl> modules = [];
    private readonly Dictionary<string, ImportDecl> imports = [];

    public IReadOnlyList<Value> Results => results;

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

    public void Print(int result)
    {
        results.Add(new Value(result));
    }

    public void Print(string result)
    {
        results.Add(new Value(result));
    }

    public void Print(Value value)
    {
        results.Add(value);
    }

    public void Print(decimal result)
    {
        results.Add(new Value(result));
    }

    public void Print(bool result)
    {
        results.Add(new Value(result));
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
        results.Add(new Value(line));
    }

    public string ReadInput()
    {
        throw new NotImplementedException();
    }

    public decimal ReadNumber()
    {
        return 0;
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