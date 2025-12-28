using Ast.Attributes;

namespace Ast.Statement;
public abstract class DeclarationStatement : Statement
{
    private AstAttribute<Runtime.ValueType> resultType;

    /// <summary>
    /// Тип результата объявления.
    /// </summary>
    public Runtime.ValueType ResultType
    {
        get => resultType.Get();

        set => resultType.Set(value);
    }
}
