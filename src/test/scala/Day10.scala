import solutions.Day10

class Day10 extends munit.FunSuite:
    test("Part 1 small example") {
        val input = "0123\n1234\n8765\n9876"
        val grid = input.split("\n").map(_.toArray)
        assertEquals(Day10.bothParts(grid)._1, 1)
    }