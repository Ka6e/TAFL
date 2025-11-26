using System.Globalization;

namespace Execution;
public class ConsoleEnvironment : IEnvironment
{
    public void AddResult(decimal result)
    {
        Console.WriteLine("Result: " + result.ToString("0.#####", CultureInfo.InvariantCulture));
    }

    public decimal ReadInt()
    {
        throw new NotImplementedException();
    }

    public void WriteInt(decimal result)
    {
        Console.WriteLine("Result: " + result.ToString("0.#####", CultureInfo.InvariantCulture));
    }
}