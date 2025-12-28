using Ast;
using Ast.Declarations;
using Ast.Expressions;
using Ast.Programm;
using Ast.Statement;

namespace Semantics.Passes;
public class AbstractPass : IAstVisitor
{
    public virtual void Visit(ProgramNode p)
    {
        p.Module.Accept(this);

        foreach (ImportDecl import in p.Imports)
        {
            import.Accept(this);
        }

        foreach (AstNode decl in p.TopLevelDecls)
        {
            decl.Accept(this);
        }
    }

    public virtual void Visit(FunctionDecl d)
    {
    }

    public virtual void Visit(ImportDecl d)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(ModuleDecl d)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(VariableExpression e)
    {
    }

    public virtual void Visit(UnaryOperationExpression e)
    {
        e.Operand.Accept(this);
    }

    public virtual void Visit(BinaryOperationExpression e)
    {
        e.Left.Accept(this);
        e.Right.Accept(this);
    }

    public virtual void Visit(BitwiseOperationExpression e)
    {
        e.Left.Accept(this);
        e.Right.Accept(this);
    }

    public virtual void Visit(LiteralExpression e)
    {
    }

    public virtual void Visit(AssignmentExpression e)
    {
    }

    public virtual void Visit(FunctionCallExpression e)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(AssignmentStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(BlockStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(IfElseStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(ForLoopStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(VariableDeclarationStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(ExpressionStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(WhileLoopStatement s)
    {
        s.Condition.Accept(this);
        s.Block.Accept(this);
    }

    public virtual void Visit(DoWhileLoopStatement s)
    {
        throw new NotImplementedException();
    }

    public virtual void Visit(BreakStatement s)
    {
    }

    public virtual void Visit(ContinueStatement s)
    {
    }

    public virtual void Visit(ReturnStatement s)
    {
        s.Value?.Accept(this);
    }
}
