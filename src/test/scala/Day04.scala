import solutions.Day04

class Day04 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX\n"
        val grid = input.strip().split("\n").map(_.toArray)
        assertEquals(Day04.part1(grid), 18)
    }