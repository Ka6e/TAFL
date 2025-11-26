namespace Execution;
public interface IEnvironment
{
    /// <summary>
    /// Вызывается после вычисления результата очередной инструкции программы.
    /// </summary>
    void AddResult(decimal result);
}
