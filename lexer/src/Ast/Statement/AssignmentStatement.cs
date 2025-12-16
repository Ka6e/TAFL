using Ast.Expressions;

namespace Ast.Statement;
public sealed class AssignmentStatement : Statement
{
    public AssignmentStatement(string name, Expression value)
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
