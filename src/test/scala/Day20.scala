import solutions.Day20

class Day20 extends munit.FunSuite:
    test("Part 2 example") {
        val input = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"
        val maze = input.split("\n").map(_.toArray)
        val (start, end) = Day20.findStartAndEnd(maze)
        assertEquals(Day20.part2(maze, end, 76), 3)
        assertEquals(Day20.part2(maze, end, 50), 32 + 31 + 29 + 39 + 25 + 23 + 20 + 19 + 12 + 14 + 12 + 22 + 4 + 3)
    }