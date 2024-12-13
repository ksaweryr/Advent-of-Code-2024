import solutions.Day13

class Day13 extends munit.FunSuite:
    test("System of equations with one solution") {
        val s = Day13.solveSystem((94, 22, 8400), (34, 67, 5400))
        assertEquals(s, Some((80, 40)))
    }

    test("System of equations with no solutions") {
        val s = Day13.solveSystem((26, 67, 12748), (66, 21, 12176))
        assertEquals(s, None)
    }

    test("System of equations with infinite solutions") {
        val s = Day13.solveSystem((1, 1, 1), (2, 2, 2))
        assertEquals(s, Some((1, 1, 1)))
    }

    test("Part 1 example") {
        val input = "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"
        val machines = input.split("\n\n").map(Day13.parseMachine(_))
        assertEquals(Day13.part1(machines), 480)
    }