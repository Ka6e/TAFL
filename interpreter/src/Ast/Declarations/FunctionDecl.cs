using Ast.Statement;

namespace Ast.Declarations;
public sealed class FunctionDecl : AstNode
{
    public FunctionDecl(
        string name,
        List<Parameter> parametrs,
        ValueType valueType,
        BlockStatement block)
    {
        Name = name;
        Parameters = parametrs;
        ValueType = valueType;
        Block = block;
    }

    public string Name { get; }

    public List<Parameter> Parameters { get; }

    public ValueType ValueType { get; }

    public BlockStatement Block { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
