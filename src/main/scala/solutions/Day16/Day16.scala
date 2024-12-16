package solutions.Day16

import scala.collection.mutable.PriorityQueue

def solve(input: String): Unit =
    val maze = input.split("\n").map(_.toArray)
    println(part1(maze))
    println(part2(maze))

def part1(maze: Array[Array[Char]]): Int =
    dijkstra(maze, Position(1, maze.length - 2, Direction.East)).filterKeys(p => p.x == maze(0).length - 2 && p.y == 1).values.min

def part2(maze: Array[Array[Char]]): Int =
    val distancesFromStart = dijkstra(maze, Position(1, maze.length - 2, Direction.East))
    val distancesFromEndE = dijkstra(maze, Position(maze(0).length - 2, 1, Direction.East))
    val distancesFromEndW = dijkstra(maze, Position(maze(0).length - 2, 1, Direction.West))
    val distancesFromEndN = dijkstra(maze, Position(maze(0).length - 2, 1, Direction.North))
    val distancesFromEndS = dijkstra(maze, Position(maze(0).length - 2, 1, Direction.South))
    val minScore = distancesFromStart.filterKeys(p => p.x == maze(0).length - 2 && p.y == 1).values.min

    (for { y <- 0 to maze.length - 1; x <- 0 to maze(0).length - 1 }
        yield
            if maze(y)(x) != '#' then
                (for { d <- Direction.values } yield
                    val pos = Position(x, y, d)
                    val invPos = Position(x, y, nextDirection(nextDirection(d)))
                    distancesFromStart.getOrElse(pos, Int.MaxValue / 2) + Seq(
                        distancesFromEndN.getOrElse(invPos, Int.MaxValue / 2),
                        distancesFromEndS.getOrElse(invPos, Int.MaxValue / 2),
                        distancesFromEndE.getOrElse(invPos, Int.MaxValue / 2),
                        distancesFromEndW.getOrElse(invPos, Int.MaxValue / 2),
                    ).min).min == minScore
            else
                false
    ).count(p => p)

def dijkstra(maze: Array[Array[Char]], startPos: Position): Map[Position, Int] =
    val ord = Ordering.by[(Position, Int), Int](-_._2)
    var pq = PriorityQueue.empty(ord)
    var distances = scala.collection.mutable.Map.empty[Position, Int]
    var visited = scala.collection.mutable.Set.empty[Position]
    pq += ((startPos, 0))
    distances += (startPos, 0)

    while !pq.isEmpty do
        val (pos, dist) = pq.dequeue()

        if !visited.contains(pos) then
            visited.add(pos)
            val (nx, ny) = nextPosition(pos.x, pos.y, pos.dir)
            for { ngb <- Seq(
                    (Position(nx, ny, pos.dir), dist + 1),
                    (Position(pos.x, pos.y, nextDirection(pos.dir)), dist + 1000),
                    (Position(pos.x, pos.y, prevDirection(pos.dir)), dist + 1000))
            } do
                if maze(ngb._1.y)(ngb._1.x) != '#' && distances.getOrElse(ngb._1, Int.MaxValue) > ngb._2 then
                    distances.update(ngb._1, ngb._2)
                    pq += ngb
                end if
        end if
    end while

    distances.toMap

enum Direction:
    case North, South, East, West

def nextDirection(d: Direction): Direction =
    d match
        case Direction.North => Direction.East
        case Direction.South => Direction.West
        case Direction.East => Direction.South
        case Direction.West => Direction.North

def prevDirection(d: Direction): Direction =
    d match
        case Direction.North => Direction.West
        case Direction.South => Direction.East
        case Direction.East => Direction.North
        case Direction.West => Direction.South

def nextPosition(x: Int, y: Int, d: Direction): (Int, Int) =
    d match
        case Direction.North => (x, y - 1)
        case Direction.South => (x, y + 1)
        case Direction.East => (x + 1, y)
        case Direction.West => (x - 1, y)
    

class Position(val x: Int, val y: Int, val dir: Direction):
    override def equals(that: Any): Boolean =
        if that.isInstanceOf[Position] then
            val rhs = that.asInstanceOf[Position]
            x == rhs.x && y == rhs.y && dir == rhs.dir
        else
            false
    override def hashCode(): Int =
        x.hashCode() * 2137 + y.hashCode() * 37 + dir.hashCode()