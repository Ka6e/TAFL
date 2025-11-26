using Execution;

namespace Parser;

/// <summary>
/// Поддельное окружение: работает как настоящее, но не совершает реального ввода/вывода.
/// </summary>
public class FakeEnvironment : IEnvironment
{
    private readonly List<decimal> results = [];

    public IReadOnlyList<decimal> Results => results;

    public void AddResult(decimal result)
    {
        results.Add(result);
    }
}