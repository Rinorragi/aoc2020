﻿namespace Aoc
open System.IO

module AocHelper =
    let readLines(filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }