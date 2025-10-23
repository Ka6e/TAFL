using System.Globalization;
using System.Runtime.CompilerServices;
using System.Text;

namespace ExampleLib;

public static class FileUtil
{
    /// <summary>
    /// Сортирует строки в указанном файле.
    /// Перезаписывает файл, но не атомарно: ошибка ввода-вывода при записи приведёт к потере данных.
    /// </summary>
    public static void SortFileLines(string path)
    {
        // Читаем и сортируем строки файла.
        List<string> lines = File.ReadLines(path, Encoding.UTF8).ToList();
        lines.Sort();

        // Перезаписываем файл с нуля (режим Truncate).
        using FileStream file = File.Open(path, FileMode.Truncate, FileAccess.Write);
        for (int i = 0, iMax = lines.Count; i < iMax; ++i)
        {
            byte[] bytes = Encoding.UTF8.GetBytes(lines[i]);
            file.Write(bytes);
            if (i != iMax - 1)
            {
                file.Write("\n"u8);
            }
        }
    }

    public static void AddLineNumbers(string path)
    {
        string text = File.ReadAllText(path);

        if (string.IsNullOrEmpty(text))
        {
            return;
        }

        string[] lines = text.Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.None);

        using FileStream file = File.Open(path, FileMode.Truncate, FileAccess.Write);
        for (int i = 0; i < lines.Length; i++)
        {
            string str = $"{i + 1}. {lines[i]}";
            byte[] bytes = Encoding.UTF8.GetBytes(str);
            file.Write(bytes);
            if (i != lines.Length - 1)
            {
                byte[] sep = Encoding.UTF8.GetBytes(Environment.NewLine);
                file.Write(sep);
            }
        }
    }
}