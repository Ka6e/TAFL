using Ast.Declarations;

namespace Ast.Programm;
public sealed class ProgramNode : AstNode
{
    public ProgramNode(
        ModuleDecl module,
        List<ImportDecl> imports,
        List<AstNode> topLevelDecls)
    {
        Module = module;
        Imports = imports;
        TopLevelDecls = topLevelDecls;
    }

    public ModuleDecl Module { get; }

    public List<ImportDecl> Imports { get; }

    public List<AstNode> TopLevelDecls { get; }

    public override void Accept(IAstVisitor visitor)
    {
        visitor.Visit(this);
    }
}
