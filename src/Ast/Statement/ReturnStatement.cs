using Ast.Expressions;

namespace Ast.Statement;
public sealed class ReturnStatement : Statement
{
    public ReturnStatement(Expression? value, Runtime.ValueType type)
    {
        Value = value;
        Type = type;
    }

    public Expression? Value { get; }

    public Runtime.ValueType Type { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
