using System.Globalization;

namespace Execution;
public class ConsoleEnvironment : IEnvironment
{
    public void AddResult(decimal result)
    {
        Console.WriteLine("Result: " + result.ToString("0.#####", CultureInfo.InvariantCulture));
    }

    public decimal ReadNumber()
    {
        decimal.TryParse(Console.ReadLine() ?? "", out decimal result);

        return result;
    }

    public void WriteNumber(decimal result)
    {
        Console.WriteLine(result.ToString("0.#####", CultureInfo.InvariantCulture));
    }
}