namespace Lexer;
public class TextScanner(string str)
{
    private readonly string str = str;
    private int postition;

    public char Peek(int n = 0)
    {
        int position = postition + n;
        return position >= str.Length ? '\0' : str[position];
    }

    public void Advance()
    {
        postition++;
    }

    public bool IsEnd()
    {
        return postition >= str.Length;
    }
}
