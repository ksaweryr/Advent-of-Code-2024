package solutions.Day02

def solve(input: String): Unit =
    val reports = input.strip.split("\n").map(row => row.split("\\s+").map(_.toInt))
    println(reports.count(isSafe))

def isSafe(report: Array[Int]): Boolean =
    val differences = report.zip(report.tail).map((a, b) => b - a)
    differences.groupBy(_.sign).size == 1 && differences.forall(d => d.abs >= 1 && d.abs <= 3)