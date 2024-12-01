package solutions.Day01

def solve(input: String): Unit =
    val (a, b) = input.strip.split("\n").map(_.split("\\s+") match { case Array(x, y, _*) => (x.toInt, y.toInt)}).toList.unzip
    val part1 = a.sorted.zip(b.sorted).map((x, y) => (x - y).abs).sum
    println(part1)
    val part2 = a.map(x => x * b.count(_ == x)).sum
    println(part2)