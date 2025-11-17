using System.Globalization;

namespace Execution;
public class ConvosleEnvironment : IEnvironment
{
    public void AddResult(double result)
    {
        Console.WriteLine("Result: " + result.ToString("0.#####", CultureInfo.InvariantCulture));
    }
}
