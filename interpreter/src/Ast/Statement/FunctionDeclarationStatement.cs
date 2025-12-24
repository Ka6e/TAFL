namespace Ast.Statement;
public sealed class FunctionDeclarationStatement : Statement
{

    public FunctionDeclarationStatement(string name)
    {
        
    }

    public override void Accept(IAstVisitor visitor)
    {
        throw new NotImplementedException();
    }
}
