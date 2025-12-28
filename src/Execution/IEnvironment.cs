using Ast.Declarations;

using Runtime;

namespace Execution;
public interface IEnvironment
{
    string ReadInput();

    void Print(Value value);

    void Print(decimal result);

    void Print(int result);

    void Print(string result);

    void Print(bool result);

    void PrintLine();

    void PrintLine(string line);

    void PrintLine(decimal line);

    void AddModule(ModuleDecl module);

    void RemoveModule(ModuleDecl module);

    void AddImport(ImportDecl import);

    void RemoveImport(ImportDecl import);
}
