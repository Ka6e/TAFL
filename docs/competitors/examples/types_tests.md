# Тесты

## ReverseString

```
module ReverseString
import IO
func main(): void {
    let s: string = readLine()
    let n: int = length(s)
    var result = ""
    var i = n - 1
    while (i >= 0) {
        result = result + s[i]
        i = i - 1
    }
    print(result)
}
```

## CheckPalindrome

```
module CheckPalindrome
import IO
func main(): void {
    let s: string = readLine()
    let n: int = length(s)
    var isPalindrome = 1   // bool (числовая семантика)
    var i = 0
    var j = n - 1
    while (i < j && isPalindrome) {
        if (toLower(s[i]) != toLower(s[j])) then
            isPalindrome = 0
        else
            i = i + 1
            j = j - 1
    }
    if (isPalindrome) then
        print("yes")
    else
        print("no")
}
```
## IsLeapYear

```
module IsLeapYear
import IO
func main(): void {
    let year: int = readInt()
    var isLeap = 0
    if (year % 400 == 0) then
        isLeap = 1
    else if (year % 100 == 0) then
        isLeap = 0
    else if (year % 4 == 0) then
        isLeap = 1
    if (isLeap) then
        print("yes")
    else
        print("no")
}
```

