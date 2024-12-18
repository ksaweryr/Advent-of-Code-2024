import solutions.Day18

class Day18 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1"
        val locations = Day18.parseLocations(input)
        assertEquals(Day18.part1(locations, 12, 7, 7), 22)
    }

    test("Part 2 example") {
        val input = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"
        val locations = Day18.parseLocations(input)
        assertEquals(Day18.part2(locations, 12, 7, 7), (6,1))
    }