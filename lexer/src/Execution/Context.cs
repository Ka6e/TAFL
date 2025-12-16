using Runtime;

namespace Execution;
public class Context
{
    private readonly Stack<Scope> scopes = [];
    private readonly Dictionary<string, Value> constants = [];

    public void PushScope(Scope scope)
    {
        scopes.Push(scope);
    }

    public void PopScope()
    {
        scopes.Pop();
    }

    public Scope GetLastScope()
    {
        return scopes.Peek();
    }

    /// <summary>
    /// Возвращает значение переменной или константы.
    /// </summary>
    public Value GetValue(string name)
    {
        foreach (Scope s in scopes)
        {
            if (s.TryGetVariable(name, out Value variable))
            {
                return variable;
            }
        }

        if (constants.TryGetValue(name, out Value? constant))
        {
            return constant;
        }

        throw new ArgumentException($"Variable '{name}' is not defined");
    }

    /// <summary>
    /// Присваивает (изменяет) значение переменной.
    /// </summary>
    public void AssignVariable(string name, Value value)
    {
        foreach (Scope s in scopes.Reverse())
        {
            if (s.TryAssignVariable(name, value))
            {
                return;
            }
        }

        throw new ArgumentException($"Variable '{name}' is not defined");
    }

    /// <summary>
    /// Определяет переменную в текущей области видимости.
    /// </summary>
    public void DefineVariable(string name, Value value)
    {
        if (!scopes.Peek().TryDefineVariable(name, value))
        {
            throw new ArgumentException($"Variable '{name}' is already defined in this scope");
        }
    }

    /// <summary>
    /// Определяет константу в глобальной области видимости.
    /// </summary>
    public void DefineConstant(string name, Value value)
    {
        if (!constants.TryAdd(name, value))
        {
            throw new ArgumentException($"Constant '{name}' is already defined");
        }
    }
}
