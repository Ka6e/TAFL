namespace Ast.Declarations;
public sealed class ImportDecl : AstNode
{
    public ImportDecl(string name)
    {
        Name = name;
    }

    public string Name { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
