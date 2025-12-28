namespace Semantics.Expeptions;

#pragma warning disable RCS1194
public class DuplicateSymbolException : Exception
{
    public DuplicateSymbolException(string name)
    : base($"The name {name} is already defined in the current scope")
    {
    }
}

#pragma warning disable RCS1194
