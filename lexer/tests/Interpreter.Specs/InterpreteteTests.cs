using Reqnroll;

using Xunit;

namespace Interpreter.Specs;

[Binding]
public class InterpreterStepDefinitions
{
    private readonly TestEnvironment environment;
    private readonly GlacierInterpreter interpreter;
    private string program = string.Empty;

    public InterpreterStepDefinitions()
    {
        environment = new TestEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Given("I enter into the console:")]
    public void GivenIEnterIntoTheConsole(DataTable table)
    {
        environment.SetInputFromTable(table);
    }

    [When("I execute the program:")]
    public void WhenIExecuteTheProgram(string code)
    {
        program = code;
        environment.ClearOutput();
        interpreter.Execute(program);
    }

    [Then("I should get the output:")]
    public void ThenIShouldGetTheOutput(string expectedOutput)
    {
        string actual = environment.Output.Trim();
        string expected = expectedOutput.Trim();

        Assert.Equal(Normalize(expected), Normalize(actual));
    }

    private string Normalize(string s)
        => s.Replace("\r\n", "\n").Trim();
}
