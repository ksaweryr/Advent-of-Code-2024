import solutions.Day18

class Day18 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1"
        val locations = Day18.parseLocations(input)
        assertEquals(Day18.part1(locations, 7, 7), 22)
    }