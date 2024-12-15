package solutions.Day15

import scala.annotation.tailrec

def solve(input: String): Unit =
    val parts = input.split("\n\n")
    val (grid, robotPos) = parseGrid(parts(0))
    val moves = parseMoves(parts(1))
    println(part1(grid, robotPos, moves))

@tailrec def part1(grid: Array[Array[Option[WarehouseObject]]], robotPos: (Int, Int), moves: List[Direction]): Int =
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

def attemptMove(grid: Array[Array[Option[WarehouseObject]]], pos: (Int, Int), direction: Direction): Option[Array[Array[Option[WarehouseObject]]]] =
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

def newPosition(pos: (Int, Int), direction: Direction): (Int, Int) =
    direction match
        case Direction.Up => (pos._1, pos._2 - 1)
        case Direction.Down => (pos._1, pos._2 + 1)
        case Direction.Left => (pos._1 - 1, pos._2)
        case Direction.Right => (pos._1 + 1, pos._2)

enum WarehouseObject:
    case Box, Wall

enum Direction:
    case Up, Down, Left, Right

def parseGrid(s: String): (Array[Array[Option[WarehouseObject]]], (Int, Int)) =
    val lines = s.split("\n")
    val robotPos = lines.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((c, x) => (c, (x, y)))).find(_._1 == '@').get._2
    val grid = lines.map(_.map(_ match {
        case '#' => Some(WarehouseObject.Wall)
        case 'O' => Some(WarehouseObject.Box)
        case _ => None
    }).toArray)
    (grid, robotPos)

def parseMoves(s: String): List[Direction] =
    s.split("\n").mkString.map(_ match
        case '^' => Direction.Up
        case 'v' => Direction.Down
        case '<' => Direction.Left
        case '>' => Direction.Right
    ).toList