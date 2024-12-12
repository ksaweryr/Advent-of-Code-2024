import solutions.Day12

class Day12 extends munit.FunSuite:
    test("Small example") {
        val grid = "AAAA\nBBCD\nBBCC\nEEEC".split("\n").map(_.toArray)
        assertEquals(Day12.calculateCost(grid, false), 140)
        assertEquals(Day12.calculateCost(grid, true), 80)
    }