using Ast.Declarations;

using Runtime;

namespace Execution;
public interface IEnvironment
{
    void AddResult(decimal result);

    void AddResult(Value result);

    decimal ReadNumber();

    string ReadInput();

    void Print(decimal result);

    void Print(string result);

    void PrintLine();

    void PrintLine(string line);

    void PrintLine(decimal line);

    void AddModule(ModuleDecl module);

    void RemoveModule(ModuleDecl module);

    void AddImport(ImportDecl import);

    void RemoveImport(ImportDecl import);
}
