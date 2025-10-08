# Инструкция по интеграции ANTLR4 в C# проект

<hr>

1. __Создание проекта__

``` bash
dotnet new console -n AntlrExample -f net8.0
cd AntlrExample
```

2. __Добавление зависимостей__

> Отредактируйте файл проекта ___.csproj___:

``` xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Antlr4.Runtime" Version="4.13.1" />
    <PackageReference Include="Antlr4BuildTasks" Version="12.0.1" PrivateAssets="all" />
  </ItemGroup>
</Project>
```

3. __Добавление грамматики__

> - 1. Создайте папку ___Grammar___
> - 2. Добавьте файл ___Math.g4___:

```antlr4
grammar Math;

expression: NUMBER ('+' NUMBER)*;

NUMBER: [0-9]+;
WS: [ \t\r\n]+ -> skip;
```

4. __Настройка генерации кода__

>Добавьте в ___.csproj___ внутри ___\<Project>___:

```xml
<ItemGroup>
  <Antlr4 Include="Grammar\Math.g4">
    <Package>MathParser</Package>
    <Visitor>True</Visitor>
  </Antlr4>
</ItemGroup>
```

5. __Проверка сборки__

```bash
dotnet build
```

_Убедитесь:_

>- Нет ошибок компиляции 
>- В папке ___obj/Debug/net8.0___ появились сгенерированные файлы ___MathParser.cs___, ___MathLexer.cs___

6. __Использование парсера__

>Замените содержимое ___Program.cs___:

```csharp
using Antlr4.Runtime;

namespace AntlrExample;

class Program
{
    static void Main()
    {
        var input = "42 + 7";
        var stream = new AntlrInputStream(input);
        var lexer = new MathLexer(stream);
        var tokens = new CommonTokenStream(lexer);
        var parser = new MathParser(tokens);
        
        var tree = parser.expression();
        Console.WriteLine($"Дерево разбора: {tree.ToStringTree(parser)}");
    }
}
```

7. __Запуск приложения__

```bash
dotnet run
```

Ожидаемый вывод:

```
Дерево разбора: (expression 42 + 7)
```

8. __Дополнительная проверка__

Для проверки цепочки токенов добавьте в ___Program.cs___:

```csharp
var lexer = new MathLexer(stream);
tokens.Fill();
foreach (var token in tokens.GetTokens())
{
    if (token.Type != -1)
        Console.WriteLine($"{MathParser.DefaultVocabulary.GetSymbolicName(token.Type)}: '{token.Text}'");
}
```

<hr>

__Примечания__
> - Грамматика сохраняется в кодировке UTF-8
> - При изменении ___.g4___-файла пересборка запускает генерацию кода автоматически
> - Сгенерированные классы находятся в пространстве имен ___MathParser___ (указано в ___\<Package>___)

Для обработки ошибок добавьте кастомный ___ErrorListener___, унаследовав от ___BaseErrorListener___.