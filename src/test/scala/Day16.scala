import solutions.Day16

class Day16 extends munit.FunSuite:
    test("Part 1 example 1") {
        val input = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
        val maze = input.split("\n").map(_.toArray)
        assertEquals(Day16.part1(maze), 7036)
    }