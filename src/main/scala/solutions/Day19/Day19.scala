package solutions.Day19

import scala.util.boundary, boundary.break

def solve(input: String): Unit =
    val (towels, patterns) = parseTowels(input)
    println(part1(towels, patterns))
    println(part2(towels, patterns))

def parseTowels(s: String): (Array[String], Array[String]) =
    val parts = s.split("\n\n")
    val towels = parts(0).split(", ")
    val patterns = parts(1).split("\n")
    (towels, patterns)

def part1(towels: Array[String], patterns: Array[String]): Int =
    patterns.count(isPatternPossible(towels, _))

def part2(towels: Array[String], patterns: Array[String]): Long =
    patterns.map(numberOfWays(towels, _)).sum

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

def numberOfWays(towels: Array[String], pattern: String): Long =
    var dp = new Array[Long](pattern.length + 1)
    dp(0) = 1L

    for i <- 1 to pattern.length do
        dp(i) = 0L
        for t <- towels do
            if t.length <= i && dp(i - t.length) != 0 && pattern.substring(i - t.length).startsWith(t) then
                dp(i) = dp(i) + dp(i - t.length)
    
    dp(pattern.length)