package solutions.Day03

def solve(input: String): Unit =
    println(part1(input))
    println(part2(input))

def part1(input: String): Int =
    val pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
    (for m <- pattern.findAllMatchIn(input)
        yield m.group(1).toInt * m.group(2).toInt).sum

def part2(input: String): Int =
    val pattern = "(?:(mul)\\((\\d{1,3}),(\\d{1,3})\\))|(?:(do)\\(\\))|(?:(don't)\\(\\))".r
    pattern.findAllMatchIn(input).foldLeft((0, 1))((acc, m) =>
        if m.group(1) != null then
            (acc._1 + m.group(2).toInt * m.group(3).toInt * acc._2, acc._2)
        else if m.group(4) != null then
            (acc._1, 1)
        else if m.group(5) != null then
            (acc._1, 0)
        else
            throw new RuntimeException("unreachable")
    )._1