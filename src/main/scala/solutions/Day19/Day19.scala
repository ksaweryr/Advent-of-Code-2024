package solutions.Day19

import scala.util.boundary, boundary.break

def solve(input: String): Unit =
    val (towels, patterns) = parseTowels(input)
    println(part1(towels, patterns))

def parseTowels(s: String): (Array[String], Array[String]) =
    val parts = s.split("\n\n")
    val towels = parts(0).split(", ")
    val patterns = parts(1).split("\n")
    (towels, patterns)

def part1(towels: Array[String], patterns: Array[String]): Int =
    patterns.count(isPatternPossible(towels, _))

def isPatternPossible(towels: Array[String], pattern: String): Boolean =
    var dp = new Array[Boolean](pattern.length + 1)
    dp(0) = true

    for i <- 1 to pattern.length do
        boundary {
            for t <- towels do
                if t.length <= i && dp(i - t.length) && pattern.substring(i - t.length).startsWith(t) then
                    dp(i) = true
                    break()
        }
    
    dp(pattern.length)