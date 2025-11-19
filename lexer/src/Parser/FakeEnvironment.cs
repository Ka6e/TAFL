using Execution;

namespace Parser;
/// <summary>
/// Поддельное окружение: работает как настоящее, но не совершает реального ввода/вывода.
/// </summary>
public class FakeEnvironment : IEnvironment
{
    private readonly List<decimal> _results = [];

    public IReadOnlyList<decimal> Results => _results;

    public void AddResult(decimal result)
    {
        _results.Add(result);
    }
}