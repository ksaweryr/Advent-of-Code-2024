import solutions.Day14

class Day14 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"
        val robots = input.split("\n").map(Day14.parseRobot(_, (11, 7)))
        assertEquals(Day14.part1(robots), 12)
    }