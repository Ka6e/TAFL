using Ast.Expressions;

namespace Ast.Statement;
public sealed class DoWhileLoopStatement : Statement
{
    public DoWhileLoopStatement(BlockStatement block, Expression expression)
    {
        Block = block;
        Expression = expression;
    }

    public BlockStatement Block { get; }

    public Expression Expression { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
