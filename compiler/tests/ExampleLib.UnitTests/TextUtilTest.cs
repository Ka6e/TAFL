using Xunit;

namespace ExampleLib.UnitTests;

public class TextUtilTest
{
    public static TheoryData<string, RgbColor> GetTheoryColor()
    {
        return new TheoryData<string, RgbColor>()
        {
            { "#fff", new RgbColor(255, 255, 255) },
            { "#ffa500", new RgbColor(255, 165, 0) },
            { "#ffffff", new RgbColor(255, 255, 255) },
            { "#FFFFFF", new RgbColor(255, 255, 255) },
        };
    }

    public static TheoryData<string> GetTheoryInvalidColor()
    {
        return new TheoryData<string>()
        {
            "#A",
            "#AA",
            "#AAAA",
            "#AAAAA",
            "#AAAAAAA",
            "abcdef",
            "#abcdeg",
        };
    }

    [Fact]
    public void Can_extract_russian_words()
    {
        const string text = """
                            Играют волны — ветер свищет,
                            И мачта гнётся и скрыпит…
                            Увы! он счастия не ищет
                            И не от счастия бежит!
                            """;
        List<string> expected =
        [
            "Играют",
            "волны",
            "ветер",
            "свищет",
            "И",
            "мачта",
            "гнётся",
            "и",
            "скрыпит",
            "Увы",
            "он",
            "счастия",
            "не",
            "ищет",
            "И",
            "не",
            "от",
            "счастия",
            "бежит",
        ];

        List<string> actual = TextUtil.ExtractWords(text);
        Assert.Equal(expected, actual);
    }

    [Fact]
    public void Can_extract_words_with_hyphens()
    {
        const string text = "Что-нибудь да как-нибудь, и +/- что- то ещё";
        List<string> expected =
        [
            "Что-нибудь",
            "да",
            "как-нибудь",
            "и",
            "что",
            "то",
            "ещё",
        ];

        List<string> actual = TextUtil.ExtractWords(text);
        Assert.Equal(expected, actual);
    }

    [Fact]
    public void Can_extract_words_with_apostrophes()
    {
        const string text = "Children's toys and three cats' toys";
        List<string> expected =
        [
            "Children's",
            "toys",
            "and",
            "three",
            "cats'",
            "toys",
        ];

        List<string> actual = TextUtil.ExtractWords(text);
        Assert.Equal(expected, actual);
    }

    [Fact]
    public void Can_extract_words_with_grave_accent()
    {
        const string text = "Children`s toys and three cats` toys, all of''them are green";
        List<string> expected =
        [
            "Children`s",
            "toys",
            "and",
            "three",
            "cats`",
            "toys",
            "all",
            "of'",
            "them",
            "are",
            "green",
        ];

        List<string> actual = TextUtil.ExtractWords(text);
        Assert.Equal(expected, actual);
    }

    [Theory]
    [MemberData(nameof(GetTheoryColor))]
    public void ParseRgbColor_ValidData_CanParse(string colorStr, RgbColor color)
    {
        RgbColor testColor = TextUtil.ParseRgbColor(colorStr);

        Assert.Equal(testColor.Red, color.Red);
        Assert.Equal(testColor.Green, color.Green);
        Assert.Equal(testColor.Blue, color.Blue);
    }

    [Theory]
    [MemberData(nameof(GetTheoryInvalidColor))]
    public void ParseRgbColor_InvalidData_FormatException(string colorStr)
    {
        Assert.Throws<FormatException>(() => TextUtil.ParseRgbColor(colorStr));
    }

    [Fact]
    public void ParseRgbColor_EmptyString_ArgumentNullException()
    {
        string emptyStr = string.Empty;

        Assert.Throws<ArgumentNullException>(() => TextUtil.ParseRgbColor(emptyStr));
    }
}