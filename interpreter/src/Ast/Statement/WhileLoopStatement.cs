using Ast.Expressions;

namespace Ast.Statement;
public sealed class WhileLoopStatement : Statement
{
    public WhileLoopStatement(Expression condition, BlockStatement block)
    {
        Condition = condition;
        Block = block;
    }

    public Expression Condition { get; }

    public BlockStatement Block { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
