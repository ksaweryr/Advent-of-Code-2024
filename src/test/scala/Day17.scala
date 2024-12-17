import solutions.Day17

class Day17 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
        val (state, program) = Day17.parse(input)
        assertEquals(Day17.part1(state, program), "4,6,3,5,6,3,5,2,1,0")
    }

    test("Part 1 small examples") {
        {
            val state = Day17.ProgramState(0, 0, 0, 9, List.empty)
            val program = Day17.parseProgram(Array(2,6))
            val result = Day17.runProgram(state, program)
            assertEquals(result.rb, 1)
        }

        {
            val state = Day17.ProgramState(0, 10, 0, 0, List.empty)
            val program = Day17.parseProgram(Array(5,0,5,1,5,4))
            val result = Day17.runProgram(state, program)
            assertEquals(result.output, List(2,1,0))
        }
    }