namespace Execution;
public interface IEnvironment
{
    /// <summary>
    /// Вызывается после вычисления результата очередной инструкции программы.
    /// </summary>
    public void AddResult(double result);
}
