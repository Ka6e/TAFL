using Ast.Programm;
using Ast.Statement;

using Execution;

namespace Interpreter;
public class GlacierInterpreter
{
    private readonly Context context;
    private readonly IEnvironment environment;

    public GlacierInterpreter(IEnvironment environment)
    {
        context = new Context();
        this.environment = environment;
    }

    public void Execute(string sourceCode)
    {
        if (string.IsNullOrEmpty(sourceCode))
        {
            throw new ArgumentException("Source code cannot be null or empty", nameof(sourceCode));
        }

        Parser.Parser parser = new(sourceCode);

        ProgramNode program = parser.ParseProgram();

        AstEvaluator evaluator = new(context, environment);
        evaluator.Evaluate(program);
    }
}
