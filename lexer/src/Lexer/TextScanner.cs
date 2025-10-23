namespace Lexer;
public class TextScanner(string str)
{
    private readonly string _str = str;
    private int _postition;

    public char Peek(int n = 0)
    {
        int position = _postition + n;
        return position >= _str.Length ? '\0' : _str[position];
    }

    public void Advance()
    {
        _postition++;
    }

    public bool IsEnd()
    {
        return _postition >= _str.Length;
    }
}
