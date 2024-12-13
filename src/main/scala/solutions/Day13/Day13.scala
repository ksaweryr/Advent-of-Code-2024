package solutions.Day13

import scala.annotation.tailrec

def solve(input: String): Unit =
    val machines = input.split("\n\n").map(parseMachine(_))
    println(part1(machines))
    println(part2(machines))

def part1(machines: Array[Machine]): Long =
    machines.map(_.requiredTokens()).sum

def part2(machines: Array[Machine]): Long =
    machines.map(_.requiredTokens2()).sum

class Machine(val a: (Long, Long), val b: (Long, Long), val prize: (Long, Long)):
    override def toString(): String =
        s"Machine(Button A: ${a}, Button B: ${b}, Prize: ${prize})"
    
    def requiredTokens(): Long =
        solveSystem((a._1, b._1, prize._1), (a._2, b._2, prize._2)) match {
            case None => 0
            case Some((x, y)) => if x <= 100 && y <= 100 then 3 * x + y else 0
            case Some((a, b, c)) => throw RuntimeException("A system with infinite solutions doesn't appear in the input")
        }
    
    def requiredTokens2(): Long =
        solveSystem((a._1, b._1, prize._1 + 10000000000000L), (a._2, b._2, prize._2 + 10000000000000L)) match {
            case None => 0
            case Some((x, y)) => 3 * x + y
            case Some((a, b, c)) => throw RuntimeException("A system with infinite solutions doesn't appear in the input")
        }

def parseMachine(s: String): Machine =
    val lines = s.split("\n")
    Machine(parseButton(lines(0)), parseButton(lines(1)), parsePrize(lines(2)))

def parseButton(s: String): (Long, Long) =
    val parts = s.split(": ")(1).split(", ")
    val x = parts(0).substring(1).toLong
    val y = parts(1).substring(1).toLong
    (x, y)

def parsePrize(s: String): (Long, Long) =
    val parts = s.split(": ")(1).split(", ")
    val x = parts(0).substring(2).toLong
    val y = parts(1).substring(2).toLong
    (x, y)

def solveSystem(e1: (Long, Long, Long), e2: (Long, Long, Long)): Option[(Long, Long) | (Long, Long, Long)] =
    val (a1, b1, c1) = e1
    val (a2, b2, c2) = e2
    val d = a1 * b2 - a2 * b1
    val dx = c1 * b2 - c2 * b1
    val dy = a1 * c2 - a2 * c1

    if d == 0 then
        if dx == 0 && dy == 0 then
            Some((a2 - a1, b2 - b1, c2 - c1))
        else
            None
    else if dx % d != 0 || dy % d != 0 then
        None
    else
        Some((dx / d, dy / d))