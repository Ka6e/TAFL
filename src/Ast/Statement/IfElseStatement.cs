using Ast.Expressions;

namespace Ast.Statement;
public sealed class IfElseStatement : Statement
{
    public IfElseStatement(Expression condition, BlockStatement thenBlock, BlockStatement? elseBlock)
    {
        Condition = condition;
        ThenBlock = thenBlock;
        ElseBlock = elseBlock;
    }

    public Expression Condition { get; }

    public BlockStatement ThenBlock { get; }

    public BlockStatement? ElseBlock { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
