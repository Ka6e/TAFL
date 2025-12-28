using System.Globalization;

using Ast.Declarations;

using Execution;

using Reqnroll;

using Runtime;

using ValueType = Runtime.ValueType;

namespace Interpreter.Specs;

public class TestEnvironment : IEnvironment
{
    private readonly Queue<string> inputQueue = new();
    private readonly Dictionary<string, ModuleDecl> modules = new();

    public List<decimal> Results { get; } = new();

    public string Output { get; private set; } = string.Empty;

    public void SetInputFromTable(Table table)
    {
        inputQueue.Clear();
        foreach (DataTableRow? row in table.Rows)
        {
            inputQueue.Enqueue(row["Value"]);
        }
    }

    public decimal ReadNumber()
    {
        string inputStr = ReadInput();
        if (decimal.TryParse(inputStr, NumberStyles.Any, CultureInfo.InvariantCulture, out decimal value))
        {
            return value;
        }

        throw new InvalidOperationException($"Cannot parse '{inputStr}' as number.");
    }

    public void AddResult(decimal result)
    {
        Results.Add(result);
    }

    public void ClearOutput() => Output = string.Empty;

    public void WriteNumber(decimal result)
    {
        string formatted = (result % 1 == 0)
            ? result.ToString("0", CultureInfo.InvariantCulture)
            : result.ToString("0.#####", CultureInfo.InvariantCulture);

        Output += formatted;
    }

    public void AddResult(Value result)
    {
        switch (result.GetValueType())
        {
            case ValueType.Int:
                WriteNumber(result.AsInt());
                break;
            case ValueType.Float:
                WriteNumber(result.AsFloat());
                break;
            default:
                throw new InvalidOperationException($"Unsupported value type: {result.GetValueType()}");
        }
    }

    public void AddModule(ModuleDecl module)
    {
        if (!modules.ContainsKey(module.Name))
        {
            modules.Add(module.Name, module);
        }
    }

    public void RemoveModule(ModuleDecl module)
    {
        throw new NotImplementedException();
    }

    public void AddImport(ImportDecl import)
    {
        throw new NotImplementedException();
    }

    public void RemoveImport(ImportDecl import)
    {
        throw new NotImplementedException();
    }

    public void Print(decimal result)
    {
        Output += result;
    }

    public void Print(string result)
    {
        Output += result;
    }

    public void PrintLine()
    {
        Console.WriteLine(Output);
    }

    public void PrintLine(string line)
    {
        Output += line;
        PrintLine();
    }

    public void PrintLine(decimal line)
    {
        throw new NotImplementedException();
    }

    public string ReadInput()
    {
        if (inputQueue.Count == 0)
        {
            throw new InvalidOperationException("No input values available.");
        }

        return inputQueue.Dequeue();
    }
}
