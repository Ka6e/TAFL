using Ast.Expressions;

using ValueType = Runtime.ValueType;

namespace Ast.Statement;
public class LetDeclarationStatement : Statement
{
    public LetDeclarationStatement(string name, ValueType type, Expression? value)
    {
        Name = name;
        Type = type;
        Value = value;
    }

    public string Name { get; }

    public ValueType Type { get; }

    public Expression? Value { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
