package solutions.Day12

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    println(calculateCost(grid, false))
    println(calculateCost(grid, true))

def calculateCost(grid: Array[Array[Char]], part2: Boolean): Int =
    grid.zipWithIndex.flatMap((row, y) => row.zipWithIndex.map((c, x) => (x, y))).foldLeft((0, Set.empty[(Int, Int)]))((acc, pos) => {
        val (totalCost, visited) = acc
        if !visited.contains(pos) then
            val (price, newVisited) = if !part2 then
                findPrice(grid, List(pos), visited + pos)
            else
                findPrice2(grid, List(pos), visited + pos)
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

enum Side:
    case Top, Bottom, Left, Right

@tailrec def findPrice2(grid: Array[Array[Char]], toVisit: List[(Int, Int)], visited: Set[(Int, Int)], acc: (Int, Set[((Int, Int), Side)]) = (0, Set.empty)): (Int, Set[(Int, Int)]) =
    toVisit match
        case head :: next => {
            val (x, y) = head
            val c = grid(y)(x)
            val neighbours = List((1, 0, Side.Right), (-1, 0, Side.Left), (0, 1, Side.Bottom), (0, -1, Side.Top))
                .map((dx, dy,side) => ((x + dx, y + dy), side))
                val newToVisit = neighbours.filter((p, _) => inBounds(grid, p) && grid(p._2)(p._1) == c && !visited.contains(p)).map(_._1)
                val newSegments = neighbours.filter((p, _) => !inBounds(grid, p) || grid(p._2)(p._1) != c)
            findPrice2(grid, newToVisit ++ next, visited ++ newToVisit.toSet, (acc._1 + 1, acc._2 ++ newSegments.toSet))
        }
        case Nil => (acc._1 * countSides(acc._2), visited)

@tailrec def countSides(segments: Set[((Int, Int), Side)], acc: Int = 0): Int =
    if segments.size == 0 then
        acc
    else
        val s = segments.find(_ => true).get
        countSides(walkEdge(List(s), segments - s), acc + 1)

@tailrec def walkEdge(toVisit: List[((Int, Int), Side)], segments: Set[((Int, Int), Side)]): Set[((Int, Int), Side)] =
    toVisit match
        case head :: next => {
            val (pos, side) = head
            val (x, y) = pos
            val deltas = side match
                case Side.Top | Side.Bottom => List((1, 0), (-1, 0))
                case Side.Left | Side.Right => List((0, 1), (0, -1))
            val newToVisit = deltas.map((dx, dy) => ((x + dx, y + dy), side)).filter(segments.contains(_))
            walkEdge(newToVisit ++ next, segments -- newToVisit.toSet)
        }
        case Nil => segments

def inBounds(grid: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < grid.size && x >= 0 && x < grid(0).size