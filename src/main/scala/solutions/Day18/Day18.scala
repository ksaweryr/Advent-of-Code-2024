package solutions.Day18

import scala.collection.mutable.PriorityQueue

def solve(input: String): Unit =
    val locations = parseLocations(input)
    println(part1(locations))
    println(part2(locations))

def parseLocations(s: String): Array[(Int, Int)] =
    s.split("\n").map(l => {
        val a = l.split(",")
        (a(0).toInt, a(1).toInt)
    })

def part1(locations: Array[(Int, Int)], toTake: Int = 1024, w: Int = 71, h: Int = 71): Int =
    val corrupted = locations.take(toTake).toSet
    var distances = scala.collection.mutable.Map.empty[(Int, Int), Int]
    var visited = scala.collection.mutable.Set.empty[(Int, Int)]
    var pq = PriorityQueue.empty[((Int, Int), Int)](Ordering.by((p, d) => -d))

    pq.enqueue(((0, 0), 0))
    distances.update((0, 0), 0)

    while !pq.isEmpty do
        val (p, d) = pq.dequeue()

        if p == (w - 1, h - 1) then
            return d
        end if

        if !visited.contains(p) then
            visited.add(p)

            val (x, y) = p
            for (dx, dy) <- Seq((0, 1), (0, -1), (1, 0), (-1, 0)) do
                val (nx, ny) = (x + dx, y + dy)

                if inBounds(nx, ny, w, h) && !corrupted.contains((nx, ny)) && distances.getOrElse((nx, ny), Int.MaxValue) > d + 1 then
                    distances.update((nx, ny), d + 1)
                    pq.enqueue(((nx, ny), d + 1))
                end if
            end for
        end if
    end while

    -1

def part2(locations: Array[(Int, Int)], from: Int = 1024, w: Int = 71, h: Int = 71): (Int, Int) =
    (for i <- from + 1 to locations.length - 1 yield
        (locations(i - 1), part1(locations, i, w, h) == -1))
        .find(_._2)
        .get
        ._1

def inBounds(x: Int, y: Int, w: Int, h: Int): Boolean =
    x >= 0 && x < w && y >= 0 && y < h