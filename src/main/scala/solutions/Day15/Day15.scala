package solutions.Day15

import scala.annotation.tailrec

def solve(input: String): Unit =
    val parts = input.split("\n\n")
    val (grid, robotPos) = parseGrid(parts(0))
    val (grid2, robotPos2) = widenGrid(grid, robotPos)
    val moves = parseMoves(parts(1))
    println(part1(grid, robotPos, moves))
    println(part2(grid2, robotPos2, moves))

type Grid = Array[Array[Option[WarehouseObject]]]

@tailrec def part1(grid: Grid, robotPos: (Int, Int), moves: List[Direction]): Int =
    moves match
        case head :: next => attemptMove(grid, robotPos, head) match
            case None => part1(grid, robotPos, next)
            case Some(newGrid) => part1(newGrid, newPosition(robotPos, head), next)
        case Nil => grid
            .zipWithIndex
            .flatMap((row, y) => row
                .zipWithIndex
                .filter(_._1 == Some(WarehouseObject.Box))
                .map((b, x) => x + y * 100))
            .sum

@tailrec def part2(grid: Grid, robotPos: (Int, Int), moves: List[Direction]): Int =
    val lastGrid = grid.map(row => row.clone)
    moves match
        case head :: next => attemptMove2(grid, robotPos, head) match
            case None => part2(lastGrid, robotPos, next)
            case Some((newGrid, _)) => part2(newGrid, newPosition(robotPos, head), next)
        case Nil => grid
            .zipWithIndex
            .flatMap((row, y) => row
                .zipWithIndex
                .filter(_._1 == Some(WarehouseObject.LeftBox))
                .map((b, x) => x + y * 100))
            .sum

def attemptMove(grid: Grid, pos: (Int, Int), direction: Direction): Option[Grid] =
    val newpos = newPosition(pos, direction)
    grid(newpos._2)(newpos._1) match
        case None => 
            var newGrid = grid
            newGrid(newpos._2)(newpos._1) = grid(pos._2)(pos._1)
            Some(newGrid)
        case Some(WarehouseObject.Box) =>
            attemptMove(grid, newpos, direction) match
                case None => None
                case Some(_newGrid) =>
                    var newGrid = _newGrid
                    newGrid(newpos._2)(newpos._1) = grid(pos._2)(pos._1)
                    Some(newGrid) 
        case Some(WarehouseObject.Wall) => None
        case _ => throw RuntimeException("Should never happen")

def attemptMove2(grid: Grid, pos: (Int, Int), direction: Direction, updated: Set[(Int, Int)] = Set.empty): Option[(Grid, Set[(Int, Int)])] =
    val newpos = newPosition(pos, direction)
    grid(newpos._2)(newpos._1) match
        case None =>
            var newGrid = grid
            newGrid(newpos._2)(newpos._1) = grid(pos._2)(pos._1)
            newGrid(pos._2)(pos._1) = None
            Some((newGrid, updated + pos + newpos))
        case Some(WarehouseObject.LeftBox) =>
            attemptMove2(grid, newpos, direction, updated) match
                case None => None
                case Some((_newGrid, newUpdated)) =>
                    var newGrid = _newGrid
                    newGrid(newpos._2)(newpos._1) = grid(pos._2)(pos._1)
                    newGrid(pos._2)(pos._1) = None
                    if (direction == Direction.Up || direction == Direction.Down) && !newUpdated.contains((newpos._1 + 1, newpos._2)) then
                        attemptMove2(newGrid, (newpos._1 + 1, newpos._2), direction, newUpdated + pos + newpos)
                    else
                        Some((newGrid, newUpdated + pos + newpos))
        case Some(WarehouseObject.RightBox) =>
            attemptMove2(grid, newpos, direction, updated) match
                case None => None
                case Some((_newGrid, newUpdated)) =>
                    var newGrid = _newGrid
                    newGrid(newpos._2)(newpos._1) = grid(pos._2)(pos._1)
                    newGrid(pos._2)(pos._1) = None
                    if (direction == Direction.Up || direction == Direction.Down) && !newUpdated.contains((newpos._1 - 1, newpos._2)) then
                        attemptMove2(newGrid, (newpos._1 - 1, newpos._2), direction, newUpdated + pos + newpos)
                    else
                        Some((newGrid, newUpdated + pos + newpos))
        case Some(WarehouseObject.Wall) => None
        case _ => throw RuntimeException("Should never happen")

def newPosition(pos: (Int, Int), direction: Direction): (Int, Int) =
    direction match
        case Direction.Up => (pos._1, pos._2 - 1)
        case Direction.Down => (pos._1, pos._2 + 1)
        case Direction.Left => (pos._1 - 1, pos._2)
        case Direction.Right => (pos._1 + 1, pos._2)

enum WarehouseObject:
    case Box, Wall, LeftBox, RightBox

enum Direction:
    case Up, Down, Left, Right

def parseGrid(s: String): (Grid, (Int, Int)) =
    val lines = s.split("\n")
    val robotPos = lines.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((c, x) => (c, (x, y)))).find(_._1 == '@').get._2
    val grid = lines.map(_.map(_ match {
        case '#' => Some(WarehouseObject.Wall)
        case 'O' => Some(WarehouseObject.Box)
        case _ => None
    }).toArray)
    (grid, robotPos)

def widenGrid(grid: Grid, startPos: (Int, Int)): (Grid, (Int, Int)) =
    (grid.map(row => row.flatMap(cell => cell match
        case None => Seq(None, None)
        case Some(WarehouseObject.Box) => Seq(Some(WarehouseObject.LeftBox), Some(WarehouseObject.RightBox))
        case Some(WarehouseObject.Wall) => Seq(Some(WarehouseObject.Wall), Some(WarehouseObject.Wall))
        case _ => throw RuntimeException("Should never happen")
    )), (startPos._1 * 2, startPos._2))

def dumpGrid(grid: Grid): String =
    grid.map(row => row.map(_ match
        case None => '.'
        case Some(WarehouseObject.Box) => 'O'
        case Some(WarehouseObject.LeftBox) => '['
        case Some(WarehouseObject.RightBox) => ']'
        case Some(WarehouseObject.Wall) => '#'
    ).mkString).mkString("\n")

def parseMoves(s: String): List[Direction] =
    s.split("\n").mkString.map(_ match
        case '^' => Direction.Up
        case 'v' => Direction.Down
        case '<' => Direction.Left
        case '>' => Direction.Right
    ).toList