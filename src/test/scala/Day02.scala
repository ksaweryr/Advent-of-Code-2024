import solutions.Day02

class Day02 extends munit.FunSuite:
    test("Part 2 already safe") {
        assert(Day02.isSafe2(Array(7, 6, 4, 2, 1)))
    }

    test("Part 2 increasing-decreasing") {
        assert(Day02.isSafe2(Array(1, 3, 2, 4, 5)))
    }