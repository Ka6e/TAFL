using System.Globalization;

using Execution;

using Reqnroll;

namespace Interpreter.Specs;

public class TestEnvironment : IEnvironment
{
    private readonly Queue<decimal> inputQueue = new();

    public List<decimal> Results { get; } = new();

    public string Output { get; private set; } = string.Empty;

    public void SetInputFromTable(Table table)
    {
        inputQueue.Clear();
        foreach (DataTableRow? row in table.Rows)
        {
            if (decimal.TryParse(row["Value"], out decimal value))
            {
                inputQueue.Enqueue(value);
            }
        }
    }

    public void SetInputValues(params decimal[] values)
    {
        inputQueue.Clear();
        foreach (decimal v in values)
        {
            inputQueue.Enqueue(v);
        }
    }

    public decimal ReadNumber()
    {
        if (inputQueue.Count == 0)
        {
            throw new InvalidOperationException("No input values available.");
        }

        return inputQueue.Dequeue();
    }

    public void AddResult(decimal result)
    {
        Results.Add(result);
    }

    public void ClearOutput() => Output = string.Empty;

    public void Clear()
    {
        Output = string.Empty;
        inputQueue.Clear();
        Results.Clear();
    }

    public void WriteNumber(decimal result)
    {
        string formatted = (result % 1 == 0)
            ? result.ToString("0", CultureInfo.InvariantCulture)
            : result.ToString("0.#####", CultureInfo.InvariantCulture);

        Output += formatted;
    }
}
