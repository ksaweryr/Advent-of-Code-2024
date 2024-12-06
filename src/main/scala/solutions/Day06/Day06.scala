package solutions.Day06

import scala.annotation.tailrec

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    val position = grid.map(_.indexOf('^', 0)).zipWithIndex.filter(_._1 != -1)(0)
    println(part1(grid, position).size)
    println(part2(grid, position))

type Position = (Int, Int)
type Direction = (Int, Int)
class State(val position: Position, val direction: Direction, val nextPosition: Position, val visited: Set[(Position, Direction)])

@tailrec def part1(grid: Array[Array[Char]], position: Position, direction: Direction = (0, -1), visited: Set[Position] = Set.empty): Set[Position] =
    val nextPos = (position._1 + direction._1, position._2 + direction._2)
    if !inBounds(grid, nextPos) then
        visited + position
    else if grid(nextPos._2)(nextPos._1) == '#' then
        part1(grid, position, nextDirection(direction), visited)
    else
        part1(grid, nextPos, direction, visited + position)

def part2(grid: Array[Array[Char]], position: Position): Int =
    part2_gen(grid, position)
        .filterNot(s => {
            val visited = s.visited
            val pos = s.nextPosition
            val dir = s.direction
            visited.contains((pos, nextDirection(dir))) || visited.contains((pos, nextDirection(nextDirection(dir)))) || visited.contains((pos, nextDirection(nextDirection(nextDirection(dir)))))
        })
        .map(s => part2_check(grid.updated(s.nextPosition._2, grid(s.nextPosition._2).updated(s.nextPosition._1, '#')), s.position, nextDirection(s.direction), s.visited))
        .count(b => b)

@tailrec def part2_gen(grid: Array[Array[Char]], position: Position, direction: Direction = (0, -1), visited: Set[(Position, Direction)] = Set.empty, acc: Stream[State] = Stream.empty): Stream[State] =
    val nextPos = (position._1 + direction._1, position._2 + direction._2)
    if !inBounds(grid, nextPos) then
        acc
    else if grid(nextPos._2)(nextPos._1) == '#' then
        part2_gen(grid, position, nextDirection(direction), visited, acc)
    else
        part2_gen(grid, nextPos, direction, visited + ((position, direction)), State(position, direction, nextPos, visited) #:: acc)

@tailrec def part2_check(grid: Array[Array[Char]], position: Position, direction: Direction = (0, -1), visited: Set[(Position, Direction)] = Set.empty): Boolean =
    val nextPos = (position._1 + direction._1, position._2 + direction._2)
    if !inBounds(grid, nextPos) then
        false
    else if visited.contains((nextPos, direction)) then
        true
    else if grid(nextPos._2)(nextPos._1) == '#' then
        part2_check(grid, position, nextDirection(direction), visited + ((position, direction)))
    else
        part2_check(grid, nextPos, direction, visited + ((position, direction)))

def inBounds(grid: Array[Array[Char]], position: Position): Boolean =
    val (x, y) = position
    x >= 0 && x < grid(0).size && y >= 0 && y < grid.size

def nextDirection(direction: Direction): Direction =
    val (x, y) = direction
    (-y, x)