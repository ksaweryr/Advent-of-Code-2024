import solutions.Day15

class Day15 extends munit.FunSuite:
    test("Part 1 small example") {
        val input = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
        val parts = input.split("\n\n")
        val (grid, robotPos) = Day15.parseGrid(parts(0))
        val moves = Day15.parseMoves(parts(1))
        assertEquals(Day15.part1(grid, robotPos, moves), 2028)
    }

    test("Part 2 small example") {
        val input = "#######\n#...#.#\n#.....#\n#..OO@#\n#..O..#\n#.....#\n#######\n\n<vv<<^^<<^^"
        val parts = input.split("\n\n")
        val (_grid, _robotPos) = Day15.parseGrid(parts(0))
        val (grid, robotPos) = Day15.widenGrid(_grid, _robotPos)
        val moves = Day15.parseMoves(parts(1))
        assertEquals(Day15.part2(grid, robotPos, moves), 618)
    }

    test("attemptMove2 push wide box") {
        val input = "....\n....\n.O..\n.O..\n.@..\n....\n\n^"
        val parts = input.split("\n\n")
        val (_grid, _robotPos) = Day15.parseGrid(parts(0))
        val (grid, robotPos) = Day15.widenGrid(_grid, _robotPos)

        val nextGrid = Day15.attemptMove2(grid, robotPos, Day15.Direction.Up)

        assert(nextGrid.isDefined)
        assertEquals(Day15.dumpGrid(nextGrid.get._1), "........\n..[]....\n..[]....\n........\n........\n........")
    }

    test("attemptMove2 push wide pyramid") {
        val input = "..#.\n.OO.\n..O@\n....\n\n<v<^"
        val parts = input.split("\n\n")
        val (_grid, _robotPos) = Day15.parseGrid(parts(0))
        val (grid, robotPos) = Day15.widenGrid(_grid, _robotPos)

        var nextGrid = Day15.attemptMove2(grid, robotPos, Day15.Direction.Left)
        assert(nextGrid.isDefined)
        nextGrid = Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 1, robotPos._2), Day15.Direction.Down)
        assert(nextGrid.isDefined)
        nextGrid = Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 1, robotPos._2 + 1), Day15.Direction.Left)
        assert(nextGrid.isDefined)
        assert(Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 2, robotPos._2 + 1), Day15.Direction.Up).isEmpty)

        assertEquals(Day15.dumpGrid(nextGrid.get._1), "....##..\n..[][]..\n...[]...\n........")
    }

    test("attemptMove2 push one side into wall") {
        val input = "..#.\n..O@\n....\n\n<v<<^"
        val parts = input.split("\n\n")
        val (_grid, _robotPos) = Day15.parseGrid(parts(0))
        val (grid, robotPos) = Day15.widenGrid(_grid, _robotPos)

        var nextGrid = Day15.attemptMove2(grid, robotPos, Day15.Direction.Left)
        assert(nextGrid.isDefined)
        nextGrid = Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 1, robotPos._2), Day15.Direction.Down)
        assert(nextGrid.isDefined)
        nextGrid = Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 1, robotPos._2 + 1), Day15.Direction.Left)
        assert(nextGrid.isDefined)
        nextGrid = Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 2, robotPos._2 + 1), Day15.Direction.Left)
        assert(nextGrid.isDefined)
        val finalGrid = nextGrid.get._1.map(row => row.clone)
        assert(Day15.attemptMove2(nextGrid.get._1, (robotPos._1 - 3, robotPos._2 + 1), Day15.Direction.Up).isEmpty)

        assertEquals(Day15.dumpGrid(finalGrid), "....##..\n...[]...\n........")
    }

    test("Part 2 large example") {
        val input = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
        val parts = input.split("\n\n")
        val (_grid, _robotPos) = Day15.parseGrid(parts(0))
        val (grid, robotPos) = Day15.widenGrid(_grid, _robotPos)
        val moves = Day15.parseMoves(parts(1))
        println(Day15.dumpGrid(grid))
        assertEquals(Day15.part2(grid, robotPos, moves), 9021)
    }