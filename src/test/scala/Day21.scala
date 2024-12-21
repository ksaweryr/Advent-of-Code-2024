import solutions.Day21

class Day21 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "029A\n980A\n179A\n456A\n379A"
        val codes = input.split("\n")

        assertEquals(Day21.part1(codes), 126384L)
    }