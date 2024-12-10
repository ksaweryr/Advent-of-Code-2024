package solutions.Day10

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    val (part1, part2) = bothParts(grid)
    println(part1)
    println(part2)

def bothParts(grid: Array[Array[Char]]): (Int, Int) =
    val (part1, part2) = grid.map(_.zipWithIndex).zipWithIndex.flatMap((p, y) => p.filter(_._1 == '0').map((h, x) => (x, y))).map(s => (trailheadScore(grid, s), trailheadRating(grid, s))).unzip
    (part1.sum, part2.sum)

def trailheadScore(grid: Array[Array[Char]], start: (Int, Int)): Int =
    dfs(grid, List(start), true)

def trailheadRating(grid: Array[Array[Char]], start: (Int, Int)): Int =
    dfs(grid, List(start), false)

@tailrec def dfs(grid: Array[Array[Char]], toVisit: List[(Int, Int)], trackVisited: Boolean, visited: Set[(Int, Int)] = Set.empty, ninesCount: Int = 0): Int =
    toVisit match {
        case (x, y) :: next => 
            val currentChar = grid(y)(x)
            if currentChar == '9' then
                dfs(grid, next, trackVisited, visited + ((x, y)), ninesCount + 1)
            else
                val newToVisit = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
                    .map((dx, dy) => (x + dx, y + dy))
                    .filter((nx, ny) => inBounds(grid, (nx, ny)) && grid(ny)(nx) == currentChar + 1 && (!trackVisited || !visited.contains((nx, ny))))
                    .toList
                dfs(grid, newToVisit ++ next, trackVisited, visited + ((x, y)), ninesCount)
        case Nil => ninesCount
    }

def inBounds(grid: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < grid.size && x >= 0 && x < grid(0).size