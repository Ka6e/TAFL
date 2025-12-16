namespace Ast.Expressions;
public sealed class FunctionCallExpression : Expression
{
    public FunctionCallExpression()
    {
        
    }

    public override void Accept(IAstVisitor visitor)
    {
        throw new NotImplementedException();
    }
}
