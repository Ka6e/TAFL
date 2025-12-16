using Ast.Expressions;

namespace Ast.Statement;
public sealed class ForLoopStatement : Statement
{
    public ForLoopStatement(string iteratorName, Expression startValue, Expression endValue, BlockStatement block)
    {
        IteratorName = iteratorName;
        StartValue = startValue;
        EndValue = endValue;
        Block = block;
    }

    public string IteratorName { get; }

    public Expression StartValue { get; }

    public Expression EndValue { get; }

    public BlockStatement Block { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
