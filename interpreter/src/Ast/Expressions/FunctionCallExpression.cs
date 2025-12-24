namespace Ast.Expressions;
public sealed class FunctionCallExpression : Expression
{

    public FunctionCallExpression(string name, List<Expression> arguments)
    {
        Name = name;
        Arguments = arguments;
    }

    public string Name { get; }

    public List<Expression> Arguments { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
