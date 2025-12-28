using Runtime;

namespace Execution;
public static class EnvironmentFunctions
{
    private static readonly Dictionary<string, EnvFunction> Functions = new()
    {
        { "print", Print },
        { "readNumber", ReadNumber },
        { "readString", ReadString },
    };

    public static bool TryInvoke(string name, List<Value> args, IEnvironment env, out Value? result)
    {
        if (Functions.TryGetValue(name, out EnvFunction? func))
        {
            result = func(args, env);
            return true;
        }

        result = default;
        return false;
    }

    private static Value Print(List<Value> args, IEnvironment env)
    {
        if (args.Count == 0)
        {
            env.Print(0);
            return new Value(0);
        }

        foreach (Value arg in args)
        {
            switch (arg.GetValueType())
            {
                case Runtime.ValueType.Int:
                case Runtime.ValueType.Float:
                    decimal num = arg.AsFloat();
                    if (num % 1m == 0)
                    {
                        env.Print((int)num);
                    }
                    else
                    {
                        env.Print(num);
                    }

                    break;
                case Runtime.ValueType.String:
                    env.Print(arg.AsString());
                    break;
                case Runtime.ValueType.Bool:
                    env.Print(arg.AsBool() ? true : false);
                    break;
            }
        }

        return new Value(0);
    }

    private static Value ReadNumber(List<Value> args, IEnvironment env)
    {
        if (args.Count > 0)
        {
            throw new ArgumentException("readNumber() takes no arguments");
        }

        string input = env.ReadInput();
        decimal value = decimal.Parse(input);

        if (!input.Contains('.') && !input.Contains(','))
        {
            return new Value((int)value);
        }

        return new Value(value);
    }

    private static Value ReadString(List<Value> args, IEnvironment env)
    {
        if (args.Count > 0)
        {
            throw new ArgumentException("readString() takes no arguments");
        }

        string input = env.ReadInput();

        return new Value(input);
    }
}
