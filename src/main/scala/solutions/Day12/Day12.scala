package solutions.Day12

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    println(part1(grid))

def part1(grid: Array[Array[Char]]): Int =
    grid.zipWithIndex.flatMap((row, y) => row.zipWithIndex.map((c, x) => (x, y))).foldLeft((0, Set.empty[(Int, Int)]))((acc, pos) => {
        val (totalCost, visited) = acc
        if !visited.contains(pos) then
            val (price, newVisited) = findPrice(grid, List(pos), visited + pos)
            (totalCost + price, newVisited)
        else
            acc
    })._1

@tailrec def findPrice(grid: Array[Array[Char]], toVisit: List[(Int, Int)], visited: Set[(Int, Int)], acc: (Int, Int) = (0, 0)): (Int, Set[(Int, Int)]) =
    toVisit match
        case head :: next => {
            val (x, y) = head
            val c = grid(y)(x)
            val newToVisit = List((1, 0), (-1, 0), (0, 1), (0, -1)).map((dx, dy) => (x + dx, y + dy)).filter(p => inBounds(grid, p) && grid(p._2)(p._1) == c)
            val newPerimeter = 4 - newToVisit.size
            val actualNewToVisit = newToVisit.filterNot(visited.contains(_))
            findPrice(grid, actualNewToVisit ++ next, visited ++ actualNewToVisit.toSet, (acc._1 + 1, acc._2 + newPerimeter))
        }
        case Nil => (acc._1 * acc._2, visited)
    

def inBounds(grid: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < grid.size && x >= 0 && x < grid(0).size