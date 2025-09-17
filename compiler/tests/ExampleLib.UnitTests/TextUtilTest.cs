using Xunit;

namespace ExampleLib.UnitTests;

public class TextUtilTest
{
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

    [Fact]
    public void ParseRgbColor_ShortHex_White()
    {
        string colorStr = "#fff";

        TextUtil.RgbColor color = TextUtil.ParseRgbColor(colorStr);

        Assert.Equal(255, color.Red);
        Assert.Equal(255, color.Green);
        Assert.Equal(255, color.Blue);
    }

    [Fact]
    public void ParseRgbColor_LongHex_Orange()
    {
        string colorStr = "#ffa500";

        TextUtil.RgbColor color = TextUtil.ParseRgbColor(colorStr);

        Assert.Equal(255, color.Red);
        Assert.Equal(165, color.Green);
        Assert.Equal(0, color.Blue);
    }

    [Fact]
    public void ParseRgbColor_UpperCase_White()
    {
        string colorStr = "#FFFFFF";

        TextUtil.RgbColor color = TextUtil.ParseRgbColor(colorStr);

        Assert.Equal(255, color.Red);
        Assert.Equal(255, color.Green);
        Assert.Equal(255, color.Blue);
    }

    [Fact]
    public void ParseRgbColor_InvalidString_Exception()
    {
        string invalidColor = "abcdef";

        Assert.Throws<FormatException>(() => TextUtil.ParseRgbColor(invalidColor));
    }

    [Fact]
    public void ParseRgbColor_EmptyString_Exception()
    {
        string emptyStr = "";

        Assert.Throws<ArgumentNullException>(() => TextUtil.ParseRgbColor(emptyStr));
    }

    [Theory]
    [InlineData("#A")]
    [InlineData("#AB")]
    [InlineData("#ABCD")]
    [InlineData("#ABCDE")]
    [InlineData("#ABCDEFF")]
    public void ParseRgbColor_InvalidStringSize_Exception(string str)
    {
        Assert.Throws<FormatException>(() => TextUtil.ParseRgbColor(str));
    }

    [Fact]
    public void ParseRgbColor_InvalidSymbol_Exception()
    {
        string str = "#abcdeg";

        Assert.Throws<FormatException>(() => TextUtil.ParseRgbColor(str));
    }
}