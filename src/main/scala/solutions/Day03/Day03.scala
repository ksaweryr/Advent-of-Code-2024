package solutions.Day03

def solve(input: String): Unit =
    val pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
    val part1 = (for m <- pattern.findAllMatchIn(input)
        yield m.group(1).toInt * m.group(2).toInt).sum
    println(part1)