namespace Execution;

#pragma warning disable RCS1194
public class TypeErrorException : Exception
{
    public TypeErrorException(string message)
    : base(message)
    {
    }

    public TypeErrorException(string category, ValueType expected, ValueType actual)
        : base($"Type mismatch: {category} must be of type {expected}, got {actual}")
    {
    }
}

#pragma warning restore RCS1194