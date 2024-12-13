package solutions.Day13

import scala.annotation.tailrec

def solve(input: String): Unit =
    val machines = input.split("\n\n").map(parseMachine(_))
    println(part1(machines))

def part1(machines: Array[Machine]): Int =
    machines.map(_.requiredTokens()).sum

class Machine(val a: (Int, Int), val b: (Int, Int), val prize: (Int, Int)):
    override def toString(): String =
        s"Machine(Button A: ${a}, Button B: ${b}, Prize: ${prize})"
    
    def requiredTokens(): Int =
        solveSystem((a._1, b._1, prize._1), (a._2, b._2, prize._2)) match {
            case None => 0
            case Some((x, y)) => if x <= 100 && y <= 100 then 3 * x + y else 0
            case Some((a, b, c)) => throw RuntimeException("A system with infinite solutions doesn't appear in the input")
        }

def parseMachine(s: String): Machine =
    val lines = s.split("\n")
    Machine(parseButton(lines(0)), parseButton(lines(1)), parsePrize(lines(2)))

def parseButton(s: String): (Int, Int) =
    val parts = s.split(": ")(1).split(", ")
    val x = parts(0).substring(1).toInt
    val y = parts(1).substring(1).toInt
    (x, y)

def parsePrize(s: String): (Int, Int) =
    val parts = s.split(": ")(1).split(", ")
    val x = parts(0).substring(2).toInt
    val y = parts(1).substring(2).toInt
    (x, y)

def solveSystem(e1: (Int, Int, Int), e2: (Int, Int, Int)): Option[(Int, Int) | (Int, Int, Int)] =
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