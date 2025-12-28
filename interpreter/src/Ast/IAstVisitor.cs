using Ast.Declarations;
using Ast.Expressions;
using Ast.Programm;
using Ast.Statement;

namespace Ast;
public interface IAstVisitor
{
    void Visit(ProgramNode p);

    void Visit(FunctionDecl d);

    void Visit(ImportDecl d);

    void Visit(ModuleDecl d);

    void Visit(VariableExpression e);

    void Visit(UnaryOperationExpression e);

    void Visit(BinaryOperationExpression e);

    void Visit(BitwiseOperationExpression e);

    void Visit(LiteralExpression e);

    void Visit(AssignmentExpression e);

    void Visit(FunctionCallExpression e);

    void Visit(AssignmentStatement s);

    void Visit(BlockStatement s);

    void Visit(IfElseStatement s);

    void Visit(ForLoopStatement s);

    void Visit(VariableDeclarationStatement s);

    void Visit(ExpressionStatement s);

    void Visit(WhileLoopStatement s);

    void Visit(DoWhileLoopStatement s);

    void Visit(BreakStatement s);

    void Visit(ContinueStatement s);

    void Visit(ReturnStatement s);
}
