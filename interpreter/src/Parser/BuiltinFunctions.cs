namespace Parser;
public static class BuiltinFunctions
{
    private static readonly Dictionary<string, Func<List<decimal>, decimal>> Functions = new()
    {
        {
            "abs", Abs
        },
        {
            "min", Min
        },
        {
            "max", Max
        },
        {
            "pow", Pow
        },
        {
            "round", Round
        },
        {
            "ceil", Ceil
        },
        {
            "floor", Floor
        },
    };

    public static decimal Invoke(string name, List<decimal> arguments)
    {
        if (!Functions.TryGetValue(name, out Func<List<decimal>, decimal>? function))
        {
            throw new AggregateException($"Unknown builtin function {name}");
        }

        return function(arguments);
    }

    private static decimal Abs(List<decimal> arguments)
    {
        return Math.Abs(arguments[0]);
    }

    private static decimal Min(List<decimal> arguments)
    {
        if (arguments.Count == 0)
        {
            throw new ArgumentException("min() requires at least one argument");
        }

        return arguments.Min();
    }

    private static decimal Max(List<decimal> arguments)
    {
        if (arguments.Count == 0)
        {
            throw new ArgumentException("max() requires at least one argument");
        }

        return arguments.Max();
    }

    private static decimal Pow(List<decimal> arguments)
    {
        if (arguments.Count < 2)
        {
            throw new ArgumentException("$Usage: pow(<number>, <exponent>)");
        }

        return (decimal)Math.Pow((double)arguments[0], (double)arguments[1]);
    }

    private static decimal Round(List<decimal> arguments)
    {
        return Math.Round(arguments[0]);
    }

    private static decimal Ceil(List<decimal> arguments)
    {
        return Math.Ceiling(arguments[0]);
    }

    private static decimal Floor(List<decimal> arguments)
    {
        return Math.Floor(arguments[0]);
    }
}
