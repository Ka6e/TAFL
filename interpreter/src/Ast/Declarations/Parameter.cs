namespace Ast.Declarations;

using ValueType = Runtime.ValueType;
public sealed class Parameter
{
    public Parameter(string name, ValueType valueType)
    {
        Name = name;
        ValueType = valueType;
    }

    public string Name { get; }

    public ValueType ValueType { get; }
}
