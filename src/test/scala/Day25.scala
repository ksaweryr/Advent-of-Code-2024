import solutions.Day25

class Day25 extends munit.FunSuite:
    test("Parse lock") {
        val part = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n....."
        val lock = Day25.parsePart(part)
        assert(lock.isLeft)
        assertEquals(lock.left.get.pins, Vector(0, 5, 3, 4, 3))
    }

    test("Parse key") {
        val part = ".....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####"
        val key = Day25.parsePart(part)
        assert(key.isRight)
        assertEquals(key.right.get.pins, Vector(5, 0, 2, 1, 3))
    }

    test("Part 1 example") {
        val input = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####"
        val (locks, keys) = Day25.parseInput(input)
        assertEquals(Day25.part1(locks, keys), 3)
    }