namespace Semantics.Symbols;

/// <summary>
/// Таблица символов, основанная на лексических областях видимости (областях действия) символов в коде.
/// </summary>
public class SymbolsTable
{
    private readonly SymbolsTable? parent;

    public SymbolsTable(SymbolsTable? parent)
    {
    }

    public SymbolsTable? Parent => parent;
}
