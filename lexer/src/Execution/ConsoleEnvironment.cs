using System.Globalization;

namespace Execution;
public class ConsoleEnvironment : IEnvironment
{
    private readonly Queue<decimal> inputQueue = new();

    public List<decimal> Results { get; } = new();

    public string Output { get; private set; } = string.Empty;

    public void AddResult(decimal result)
    {
        Results.Add(result);
    }

    public decimal ReadNumber()
    {
        if (inputQueue.Count == 0)
        {
            throw new InvalidOperationException("No input values available.");
        }

        return inputQueue.Dequeue();
    }

    public void WriteNumber(decimal result)
    {
        string formatted = (result % 1 == 0)
            ? result.ToString("0", CultureInfo.InvariantCulture)
            : result.ToString("0.#####", CultureInfo.InvariantCulture);

        Output += formatted;

        Console.WriteLine(Output);
    }
}