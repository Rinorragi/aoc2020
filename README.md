# Advent of Code 2020 repository to train F\#

## Memo about solution setup

```powershell
dotnet new gitignore
dotnet new sln -o AoC2020
dotnet new classlib -lang "F#" -o src/AocHelper
dotnet sln add .\src\AocHelper\AocHelper.fsproj
dotnet new console -lang "F#" -o src/DayN
dotnet add .\src\DayN\DayN.fsproj reference .\src\AocHelper\AocHelper.fsproj
dotnet sln add .\src\DayN\DayN.fsproj
```

## Test how it works
Assuming you are at root of aoc2020 folder:

```powershell
dotnet build
dotnet run --project .\src\DayN\DayN.fsproj
``` 