import solutions.Day07

class Day07 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
        val equations = input.split("\n").map(Day07.parseEquation)
        assertEquals(Day07.part1(equations), 3749L)
    }