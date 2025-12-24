namespace Ast.Expressions;
public sealed class BitwiseOperationExpression : Expression
{
    public BitwiseOperationExpression(Expression left, BitwiseOperation operation, Expression right)
    {
        Left = left;
        Operation = operation;
        Right = right;
    }

    public Expression Left { get; }

    public BitwiseOperation Operation { get; }

    public Expression Right { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
