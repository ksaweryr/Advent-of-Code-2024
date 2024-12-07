package solutions.Day07

def solve(input: String): Unit =
    val equations = input.split("\n").map(parseEquation)
    println(part1(equations))
    println(part2(equations))

def part1(equations: Array[Equation]): Long =
    equations.filter(_.satisfiable()).map(_.result).sum

def part2(equations: Array[Equation]): Long =
    equations.filter(_.satisfiable2()).map(_.result).sum

class Equation(val result: Long, val operands: List[Long]):
    def satisfiable(acc: Option[Long] = None): Boolean =
        if acc.getOrElse(-1L) > result then
            false
        else
            operands match {
                case head :: next => Equation(result, next).satisfiable(Some(acc.getOrElse(0L) + head)) || Equation(result, next).satisfiable(Some(acc.getOrElse(1L) * head))
                case Nil => result == acc.getOrElse(-1L)
            }
    
    def satisfiable2(acc: Option[Long] = None): Boolean =
        if acc.getOrElse(-1L) > result then
            false
        else
            operands match {
                case head :: next => Equation(result, next).satisfiable2(Some(acc.getOrElse(0L) + head)) || Equation(result, next).satisfiable2(Some(acc.getOrElse(1L) * head)) || Equation(result, next).satisfiable2(Some(acc.getOrElse(0L) * Math.pow(10, Math.log10(head).toInt + 1).toLong + head))
                case Nil => result == acc.getOrElse(-1L)
            }

def parseEquation(line: String): Equation =
    val Array(a, b) = line.split(": ")
    Equation(a.toLong, b.split(" ").map(_.toLong).toList)