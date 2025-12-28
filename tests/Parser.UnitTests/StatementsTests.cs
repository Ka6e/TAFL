using System.Reflection.PortableExecutable;

using Interpreter;

using Runtime;

using Xunit.Sdk;

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
    public void Parse_ForLoop_With_Positive_Cases(string expression, List<Value> expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        IReadOnlyList<Value> actual = environment.Results;

        for (int i = 0, iMax = Math.Min(expected.Count, actual.Count); i < iMax; i++)
        {
            Assert.Equal(expected[i].AsInt(), actual[i].AsInt());
        }

        if (expected.Count != actual.Count)
        {
            Assert.Fail(
                $"Actual results count does not match expected. Expected: {expected.Count}, Actual: {actual.Count}."
            );
        }
    }

    public static TheoryData<string, List<Value>> GetForPositiveData()
    {
        return new TheoryData<string, List<Value>>()
        {
            {
                "for i = 0, i < 10, i = i + 1 in {" +
                "print(i);" +
                "}", [new Value(0), new Value(1), new Value(2), new Value(3), new Value(4), new Value(5), new Value(6), new Value(7), new Value(8), new Value(9)]
            },
            {
                "var sum = 0;" +
                "for i = 0, i < 3, i = i + 1 in {" +
                "for j = 0, j < 2, j = j + 1 in {" +
                "sum = sum + i * j;" +
                "print(sum);" +
                "}" +
                "}", [new Value(0), new Value(0), new Value(0), new Value(1), new Value(1), new Value(3)]
            },
            {
                "for i = 3, i != 0, i = i - 1 in {print(i);}", [new Value(3), new Value(2), new Value(1)]
            },
            {
                "for i = 0, i < 5, i = i + 1 in {" +
                "if i == 2 then { break; } print(i);" +
                "}", [new Value(0), new Value(1)]
            },
            {
                "for i = 0, i < 5, i = i + 1 in {" +
                "if i == 2 then { continue; } print(i);" +
                "}", [new Value(0), new Value(1), new Value(3), new Value(4)]
            },
            {
                "var x = 0;" +
                "for , , in {" +
                "x = x + 1; " +
                "if x == 10 then { break; } print(x);" +
                "}", [new Value(1), new Value(2), new Value(3), new Value(4), new Value(5), new Value(6), new Value(7), new Value(8), new Value(9)]
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
    public void Parse_Functions_With_Positive_Data(string expression, Value expected)
    {
        string code = "module Main\n" + expression;

        interpreter.Execute(code);

        Assert.Equal(expected.AsInt(), environment.Results[0].AsInt());
    }

    public static TheoryData<string, Value> GetFunctionsPositiveData()
    {
        return new TheoryData<string, Value>()
        {
            {
                "func foo(): int { return 20; }" +
                "print(foo());", new Value(20)
            },
            {
                "func inc(x: int): int { return x + 1; } " +
                "print(inc(5));", new Value(6)
            },
            {
                "func sum(a: int, b: int): int { return a + b; }" +
                "print(sum(5, 6));", new Value(11)
            },
            {
                "func foo(x: int): int { if x == 5 then { return x; } return foo(x - 1); }" +
                "print(foo(7));", new Value(5)
            },
        };
    }

    [Theory]
    [MemberData(nameof(GetIfPositiveData))]
    public void Parse_If_Positive_Data(string condition, Value expected)
    {
        string code = "module Main\n" + condition;

        interpreter.Execute(code);

        Assert.Equal(expected.AsInt(), environment.Results[0].AsInt());
    }

    public static TheoryData<string, Value> GetIfPositiveData()
    {
        return new TheoryData<string, Value>()
        {
            {
                "if 1 < 2 then { print(5); }", new Value(5)
            },
            {
                "if 1 > 2 then { print(5); } else { print(0); }", new Value(0)
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
    public void Parse_While_Positive_Data(string loop, List<Value> expected)
    {
        string code = "module Main\n" + loop;

        interpreter.Execute(code);

        IReadOnlyList<Value> actual = environment.Results;

        for (int i = 0, iMax = Math.Min(expected.Count, actual.Count); i < iMax; i++)
        {
            Assert.Equal(expected[i].AsInt(), actual[i].AsInt());
        }

        if (expected.Count != actual.Count)
        {
            Assert.Fail(
                $"Actual results count does not match expected. Expected: {expected.Count}, Actual: {actual.Count}."
            );
        }
    }

    public static TheoryData<string, List<Value>> GetWhileLooPositivepData()
    {
        return new TheoryData<string, List<Value>>()
        {
            {
                "var i = 0; while (i < 2) { i = i + 1; print(i); }", [new Value(1), new Value(2)]
            },
            {
                "var i = 0; while (i < 5) { if (i == 3) then { break; } i = i + 1; print(i); }", [new Value(1), new Value(2), new Value(3)]
            },
            {
                "var i = 0; while (i < 5) { i = i + 1; if( i == 3 ) then { continue; }  print(i); }", [new Value(1), new Value(2), new Value(4), new Value(5)]
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
    public void Parse_Do_While_Loop_Positive_Data(string loop, List<Value> expected)
    {
        string code = "module Main\n" + loop;

        interpreter.Execute(code);
        IReadOnlyList<Value> actual = environment.Results;

        for (int i = 0, iMax = Math.Min(expected.Count, actual.Count); i < iMax; i++)
        {
            Assert.Equal(expected[i].AsInt(), actual[i].AsInt());
        }

        if (expected.Count != actual.Count)
        {
            Assert.Fail(
                $"Actual results count does not match expected. Expected: {expected.Count}, Actual: {actual.Count}."
            );
        }
    }

    public static TheoryData<string, List<Value>> GetDoWhileLoopPositiveData()
    {
        return new TheoryData<string, List<Value>>()
        {
            {
                "var x = 5; do { var a = x + 5; print(a); } while (x != 5);", [new Value(10)]
            },
            {
                "var x = 0; do { x = x + 1; print(x); } while (x < 5);", [new Value(1), new Value(2), new Value(3), new Value(4), new Value(5)]
            },
            {
                "var x = 0; do { x = x + 1; if x == 3 then { break; } print(x); } while (x != 5);", [new Value(1), new Value(2)]
            },
            {
                "var x = 0; do { x = x + 1; if x == 3 then { continue; } print(x); } while (x != 5);", [new Value(1), new Value(2), new Value(4), new Value(5)]
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

    [Fact]
    public void Parse_Function_String_Data()
    {
        string code = "module Main\n func foo(): string { return \"abc\"; } print(foo());";

        interpreter.Execute(code);

        string actual = environment.Results[0].AsString();
        string expected = "abc";

        Assert.Equal(actual, expected);
    }

    [Fact]
    public void Parse_Function_Bool_Data()
    {
        string code = "module Main\n func foo(): bool { return true; } print(foo());";

        interpreter.Execute(code);

        bool actual = environment.Results[0].AsBool();
        bool expected = true;

        Assert.Equal(actual, expected);
    }

    [Fact]
    public void Parse_Var_Change_Type()
    {
        string code = "module Main\n var x = 10; x = \"hello\"; print(x);";

        interpreter.Execute(code);

        string actual = environment.Results[0].AsString();
        string expected = "hello";

        Assert.Equal(actual, expected);
    }

    [Theory]
    [MemberData(nameof(InvalidData))]
    public void Cannot_Parse_Invalid_Data(string code)
    {
        string glacierCode = "module Main\n" + code;

        Assert.ThrowsAny<Exception>(() => interpreter.Execute(glacierCode));
    }

    public static TheoryData<string> InvalidData()
    {
        return new TheoryData<string>()
        {
            {
                "let x: int = 10;" +
                "x = 5;"
            },
            {
                "let x: int = \"hello\";"
            },
        };
    }
}
