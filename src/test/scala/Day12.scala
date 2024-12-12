import solutions.Day12

class Day12 extends munit.FunSuite:
    test("Part 1 small example") {
        val grid = "AAAA\nBBCD\nBBCC\nEEEC".split("\n").map(_.toArray)
        assertEquals(Day12.part1(grid), 140)
    }