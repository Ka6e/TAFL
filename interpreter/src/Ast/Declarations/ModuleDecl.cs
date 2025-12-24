namespace Ast.Declarations;
public sealed class ModuleDecl : AstNode
{
    public ModuleDecl(string name)
    {
        Name = name;
    }

    public string Name { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
