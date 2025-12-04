using System.Globalization;

namespace Execution;
public interface IEnvironment
{
    void AddResult(decimal result);

    decimal ReadNumber();

    void WriteNumber(decimal result);
}
