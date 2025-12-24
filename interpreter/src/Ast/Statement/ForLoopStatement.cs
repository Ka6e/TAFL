using Ast.Expressions;

namespace Ast.Statement;
public sealed class ForLoopStatement : Statement
{
    public ForLoopStatement(VariableDeclarationStatement? init, Expression? condition, AssignmentStatement? step, BlockStatement block)
    {
        Init = init;
        Condition = condition;
        Step = step;
        Block = block;
    }

    public VariableDeclarationStatement? Init { get; }

    public Expression? Condition { get; }

    public AssignmentStatement? Step { get; }

    public BlockStatement Block { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
