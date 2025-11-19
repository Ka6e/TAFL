using System.Globalization;
using System.Reflection.Metadata;

using Execution;

namespace Parser.UnitTests;
public class ParseTopLevelStatementsTests
{
    private const int Precision = 5;

    private readonly Context context;
    private readonly FakeEnvironment environment;

    public ParseTopLevelStatementsTests()
    {
        context = new Context();
        environment = new FakeEnvironment();
    }

    [Theory]
    [MemberData(nameof(GetTopLevelStatementData))]
    public void Can_parse_top_level_statements(string code, List<decimal> expected)
    {

    }

    public static TheoryData<string, List<decimal>> GetTopLevelStatementData()
    {

    }
}
