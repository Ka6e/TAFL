namespace Ast.Statement;
public abstract class AbstractVariableDeclaration : DeclarationStatement
{
    protected AbstractVariableDeclaration(string name)
    {
        Name = name;
    }

    public string Name { get; }
}
