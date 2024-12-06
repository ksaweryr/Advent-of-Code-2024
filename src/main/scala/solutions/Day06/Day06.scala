package solutions.Day06

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    val position = grid.map(_.indexOf('^', 0)).zipWithIndex.filter(_._1 != -1)(0)
    println(part1(grid, position))

@tailrec def part1(grid: Array[Array[Char]], position: (Int, Int), direction: (Int, Int) = (0, -1), visited: Set[(Int, Int)] = Set.empty): Int =
    val nextPos = (position._1 + direction._1, position._2 + direction._2)
    if !inBounds(grid, nextPos) then
        (visited + position).size
    else if grid(nextPos._2)(nextPos._1) == '#' then
        part1(grid, position, nextDirection(direction), visited)
    else
        part1(grid, nextPos, direction, visited + position)

def inBounds(grid: Array[Array[Char]], position: (Int, Int)): Boolean =
    val (x, y) = position
    x >= 0 && x < grid(0).size && y >= 0 && y < grid.size

def nextDirection(direction: (Int, Int)): (Int, Int) =
    val (x, y) = direction
    (-y, x)