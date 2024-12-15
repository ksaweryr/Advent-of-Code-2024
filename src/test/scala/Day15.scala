import solutions.Day15

class Day15 extends munit.FunSuite:
    test("Part 1 small example") {
        val input = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
        val parts = input.split("\n\n")
        val (grid, robotPos) = Day15.parseGrid(parts(0))
        val moves = Day15.parseMoves(parts(1))
        assertEquals(Day15.part1(grid, robotPos, moves), 2028)
    }