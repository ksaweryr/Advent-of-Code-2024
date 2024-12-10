package solutions.Day10

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    println(part1(grid))

def part1(grid: Array[Array[Char]]): Int =
    grid.map(_.zipWithIndex).zipWithIndex.flatMap((p, y) => p.filter(_._1 == '0').map((h, x) => (x, y))).map(trailheadScore(grid, _)).sum

def trailheadScore(grid: Array[Array[Char]], start: (Int, Int)): Int =
    dfs(grid, List(start))

@tailrec def dfs(grid: Array[Array[Char]], toVisit: List[(Int, Int)], visited: Set[(Int, Int)] = Set.empty, ninesCount: Int = 0): Int =
    toVisit match {
        case (x, y) :: next => 
            val currentChar = grid(y)(x)
            if currentChar == '9' then
                dfs(grid, next, visited + ((x, y)), ninesCount + 1)
            else
                val newToVisit = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
                    .map((dx, dy) => (x + dx, y + dy))
                    .filter((nx, ny) => inBounds(grid, (nx, ny)) && grid(ny)(nx) == currentChar + 1 && !visited.contains((nx, ny)))
                    .toList
                dfs(grid, newToVisit ++ next, visited + ((x, y)), ninesCount)
        case Nil => ninesCount
    }

def inBounds(grid: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < grid.size && x >= 0 && x < grid(0).size