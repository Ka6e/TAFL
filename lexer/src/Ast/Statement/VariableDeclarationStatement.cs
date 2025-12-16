using Ast.Expressions;

namespace Ast.Statement;
public sealed class VariableDeclarationStatement : Statement
{
    public VariableDeclarationStatement(string name, Expression value)
    {
        Name = name;
        Value = value;
    }

    public string Name { get; }

    public Expression Value { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
