package solutions.Day20

def solve(input: String): Unit =
    val maze = input.split("\n").map(_.toArray)
    val (start, end) = findStartAndEnd(maze)
    println(part1(maze, end))

def findStartAndEnd(maze: Array[Array[Char]]): ((Int, Int), (Int, Int)) =
    val indexed = maze.zipWithIndex.flatMap((row, y) => row.zipWithIndex.map((c, x) => (c, (x, y))))
    val start = indexed.find(_._1 == 'S').get._2
    val end = indexed.find(_._1 == 'E').get._2
    (start, end)

def part1(maze: Array[Array[Char]], end: (Int, Int)): Int =
    val distancesFromEnd = distancesFrom(maze, end)
    findCheats(maze, distancesFromEnd).size

def distancesFrom(maze: Array[Array[Char]], start: (Int, Int)): Map[(Int, Int), Int] =
    val distances = scala.collection.mutable.Map.empty[(Int, Int), Int]
    val q = scala.collection.mutable.Queue.empty[(Int, Int)]

    distances.update(start, 0)
    q.enqueue(start)
    
    while !q.isEmpty do
        val (x, y) = q.dequeue()

        for (dx, dy) <- Seq((0, 1), (0, -1), (1, 0), (-1, 0)) do
            val (nx, ny) = (x + dx, y + dy)
            if inBounds(maze, (nx, ny)) && maze(ny)(nx) != '#' && !distances.contains((nx, ny)) then
                distances.update((nx, ny), distances((x, y)) + 1)
                q.enqueue((nx, ny))

    distances.toMap

def findCheats(maze: Array[Array[Char]], distancesFromEnd: Map[(Int, Int), Int]): Set[((Int, Int), (Int, Int))] =
    (for y <- 0 to maze.length - 1; x <- 0 to maze(0).length - 1 yield
        if maze(y)(x) != '#' then
            Set.empty[((Int, Int), (Int, Int))]
        else
            val neighbours = for (dx, dy) <- Seq((0, 1), (0, -1), (1, 0), (-1, 0)) if {
                val (nx, ny) = (x + dx, y + dy)
                inBounds(maze, (nx, ny)) && maze(ny)(nx) != '#'
            } yield (x + dx, y + dy)

            (for (x0, y0) <- neighbours; (x1, y1) <- neighbours if {
                distancesFromEnd.contains((x0, y0)) && distancesFromEnd.contains((x1, y1)) && distancesFromEnd((x1, y1)) - distancesFromEnd((x0, y0)) >= 101
            } yield ((x0, y0), (x1, y1))).toSet
    ).flatten.toSet

def inBounds(maze: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < maze.length && x >= 0 && x < maze(0).length