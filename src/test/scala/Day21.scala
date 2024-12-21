import solutions.Day21

class Day21 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "029A\n980A\n179A\n456A\n379A"
        val codes = input.split("\n")

        assertEquals(Day21.directionalToHigherLevel("^^<<A".toStream).length, Day21.directionalToHigherLevel("<<^^A".toStream).length)

        assertEquals(Day21.part1SingleCode(codes(0)), 68 * 29)
        assertEquals(Day21.part1SingleCode(codes(1)), 60 * 980)
        assertEquals(Day21.part1SingleCode(codes(2)), 68 * 179)
        assertEquals(Day21.part1SingleCode(codes(3)), 64 * 456)
        assertEquals(Day21.part1SingleCode(codes(4)), 64 * 379)

        assertEquals(Day21.part1(codes), 126384)
    }