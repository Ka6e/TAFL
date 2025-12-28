using Interpreter;

namespace Parser.UnitTests.StatementsTests;
public class StatementsTests
{
    private readonly FakeEnvironment environment;
    private readonly GlacierInterpreter interpreter;

    public StatementsTests()
    {
        environment = new FakeEnvironment();
        interpreter = new GlacierInterpreter(environment);
    }

    [Theory]
    [MemberData(nameof(GetForPositiveData))]
    public void Parse_ForLoop_With_Positive_Cases(string expression, decimal[] expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetForPositiveData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "for i = 0, i < 10, i = i + 1 in {" +
                "print(i);" +
                "}", [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            },
            {
                "var sum = 0;" +
                "for i = 0, i < 3, i = i + 1 in {" +
                "for j = 0, j < 2, j = j + 1 in {" +
                "sum = sum + i * j;" +
                "print(sum);" +
                "}" +
                "}", [0, 0, 0, 1, 1, 3]
            },
            {
                "for i = 3, i != 0, i = i - 1 in {print(i);}", [3, 2, 1]
            },
            {
                "for i = 0, i < 5, i = i + 1 in {" +
                "if i == 2 then { break; } print(i);" +
                "}", [0, 1]
            },
            {
                "for i = 0, i < 5, i = i + 1 in {" +
                "if i == 2 then { continue; } print(i);" +
                "}", [0, 1, 4]
            },
            {
                "var x = 0;" +
                "for , , in {" +
                "x = x + 1; " +
                "if x == 10 then { break; } print(x);" +
                "}", [1, 2, 3, 4, 5, 6, 7, 8, 9]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetFunctionsNegativeData))]
    public void Parse_Functions_With_Negative_Data(string expression)
    {
        string code = "module Main\n" + expression;

        Assert.Throws<ArgumentException>(() => interpreter.Execute(code));
    }

    public static TheoryData<string> GetFunctionsNegativeData()
    {
        return new TheoryData<string>()
        {
            {
                "func sum(a: int, b: int): int { return a + b; }" +
                "sum(4);"
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetFunctionsPositiveData))]
    public void Parse_Functions_With_Positive_Data(string expression, decimal[] expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetFunctionsPositiveData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "func foo(): int { return 20; }" +
                "print(foo());", [20]
            },
            {
                "func inc(x: int): int { return x + 1; } " +
                "print(inc(5));", [6]
            },
            {
                "func sum(a: int, b: int): int { return a + b; }" +
                "print(sum(5, 6));", [11]
            },
            {
                "func foo(x: int): int { if x == 5 then { return x; } return foo(x - 1); }" +
                "print(foo(7));", [5]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetIfPositiveData))]
    public void Parse_If_Positive_Data(string condition, decimal[] expected)
    {
        string code = "module Main\n" + condition;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetIfPositiveData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "if 1 < 2 then { print(5); }", [5]
            },
            {
                "if 1 > 2 then { print(5); } else { print(0); }", [0]
            },
        };
    }

    [Fact]
    public void Parse_If_Negative_Data()
    {
        string code = "module Main\n" +
            "var x = 5;" +
            "if 1 < 2 then if x == 3 then print(3); " +
            "else { print(2); }";

        Assert.ThrowsAny<Exception>(() => interpreter.Execute(code));
    }

    [Theory]
    [MemberData(nameof(GetWhileLooPositivepData))]
    public void Parse_While_Positive_Data(string loop, decimal[] expected)
    {
        string code = "module Main\n" + loop;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetWhileLooPositivepData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "var i = 0; while (i < 2) { i = i + 1; print(i); }", [1, 2]
            },
            {
                "var i = 0; while (i < 5) { if (i == 3) then { break; } i = i + 1; print(i); }", [1, 2, 3]
            },
            {
                "var i = 0; while (i < 5) { i = i + 1; if( i == 3 ) then { continue; }  print(i); }", [1, 2, 4, 5]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetWhileLoopNegativeData))]
    public void Cannot_Parse_While_Loop_Negative_Data(string loop)
    {
        string code = "module Main\n" + loop;

        Assert.Throws<ArgumentException>(() => interpreter.Execute(code));
    }

    public static TheoryData<string> GetWhileLoopNegativeData()
    {
        return new TheoryData<string>()
        {
            {
                "var i = 0; while(i < 2) { i = i + 1; } break;"
            },
            {
                "var i = 0; while(i < 2) { i = i + 1; } continue;"
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetDoWhileLoopPositiveData))]
    public void Parse_Do_While_Loop_Positive_Data(string loop, decimal[] expected)
    {
        string code = "module Main\n" + loop;

        interpreter.Execute(code);

        Assert.Equal(expected, environment.Results);
    }

    public static TheoryData<string, decimal[]> GetDoWhileLoopPositiveData()
    {
        return new TheoryData<string, decimal[]>()
        {
            {
                "var x = 5; do { var a = x + 5; print(a); } while (x != 5);", [10]
            },
            {
                "var x = 0; do { x = x + 1; print(x); } while (x < 5);", [1, 2, 3, 4, 5]
            },
            {
                "var x = 0; do { x = x + 1; if x == 3 then { break; } print(x); } while (x != 5);", [1, 2]
            },
            {
                "var x = 0; do { x = x + 1; if x == 3 then { continue; } print(x); } while (x != 5);", [1, 2, 4, 5]
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetDoWhileLoopNegativeData))]
    public void Parse_Do_While_Loop_Negative_Data(string loop)
    {
        string code = "module Main\n" + loop;

        Assert.Throws<ArgumentException>(() => interpreter.Execute(code));
    }

    public static TheoryData<string> GetDoWhileLoopNegativeData()
    {
        return new TheoryData<string>()
        {
            {
                "var x = 5; do { var a = x + 5; print(a); } while (x != 5); break;"
            },
            {
                "var x = 5; do { var a = x + 5; print(a); } while (x != 5); continue;"
            },
        };
    }
}
