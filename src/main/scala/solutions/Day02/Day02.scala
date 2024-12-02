package solutions.Day02

import scala.util.Try

def solve(input: String): Unit =
    val reports = input.strip.split("\n").map(row => row.split("\\s+").map(_.toInt))
    println(reports.count(isSafe))
    println(reports.count(isSafe2))

def isSafe(report: Array[Int]): Boolean =
    val differences = report.zip(report.tail).map((a, b) => b - a)
    differences.groupBy(_.sign).size == 1 && differences.forall(d => d.abs >= 1 && d.abs <= 3)

def isSafe2(report: Array[Int]): Boolean =
    (for i <- 0 to report.size
        yield report.zipWithIndex.filter(_._2 != i).map(_._1)).exists(isSafe)