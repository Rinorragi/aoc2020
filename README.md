# Advent of Code 2020 repository to train F\#

## Memo about solution setup

```powershell
dotnet new gitignore
dotnet new sln -o AoC2020
dotnet new classlib -lang "F#" -o src/AocHelper
dotnet sln add .\src\AocHelper\AocHelper.fsproj
dotnet new console -lang "F#" -o src/Day1
dotnet add .\src\Day1\Day1.fsproj reference .\src\AocHelper\AocHelper.fsproj
dotnet sln add .\src\Day1\Day1.fsproj
dotnet new console -lang "F#" -o src/Day2
dotnet add .\src\Day2\Day2.fsproj reference .\src\AocHelper\AocHelper.fsproj
dotnet sln add .\src\Day2\Day1.fsproj
dotnet new console -lang "F#" -o src/Day3
dotnet add .\src\Day3\Day3.fsproj reference .\src\AocHelper\AocHelper.fsproj
dotnet sln add .\src\Day3\Day3.fsproj
```

## Test how it works
Assuming you are at root of aoc2020 folder:

```powershell
dotnet build
dotnet run --project .\src\Day1\Day1.fsproj
dotnet run --project .\src\Day2\Day2.fsproj
dotnet run --project .\src\Day3\Day3.fsproj
``` 