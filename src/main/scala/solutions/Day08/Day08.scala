package solutions.Day08

def solve(input: String): Unit =
    val grid = input.split("\n").map(_.toArray)
    val antennas = grid.zipWithIndex.flatMap((r, y) => r.zipWithIndex.filter(_._1 != '.').map((c, x) => Antenna(c, (x, y)))).groupBy(_.frequency)
    val part1 = antennas.values.toSet.flatMap(as => (for { x <- as; y <- as } yield if x == y then Set.empty[(Int, Int)] else antinodes(x, y)).flatten).filter(inBounds(grid, _))
    println(part1.size)
    val part2 = antennas.values.toSet.flatMap(as => (for { x <- as; y <- as } yield if x == y then Set.empty[(Int, Int)] else antinodes2(grid, x, y)).flatten)
    println(part2.size)

def antinodes(a1: Antenna, a2: Antenna): Set[(Int, Int)] =
    val (x1, y1) = a1.position
    val (x2, y2) = a2.position
    val (dx, dy) = (x1 - x2, y1 - y2)
    Set((x1 + dx, y1 + dy), (x2 - dx, y2 - dy))

def antinodes2(grid: Array[Array[Char]], a1: Antenna, a2: Antenna): Set[(Int, Int)] =
    val (x1, y1) = a1.position
    val (x2, y2) = a2.position
    val (dx, dy) = (x1 - x2, y1 - y2)
    val s1 = Seq.unfold((x1, y1))((sx, sy) => {
        if inBounds(grid, (sx, sy)) then
            val (nx, ny) = (sx + dx, sy + dy)
            Some(((sx, sy), (nx, ny)))
        else
            None
    }).toSet
    val s2 = Seq.unfold((x1, y1))((sx, sy) => {
        if inBounds(grid, (sx, sy)) then
            val (nx, ny) = (sx - dx, sy - dy)
            Some(((sx, sy), (nx, ny)))
        else
            None
    }).toSet
    s1 ++ s2

class Antenna(val frequency: Char, val position: (Int, Int)):
    override def toString(): String =
        s"Antenna(${frequency}, ${position})"

def inBounds(grid: Array[Array[Char]], pos: (Int, Int)): Boolean =
    val (x, y) = pos
    y >= 0 && y < grid.size && x >= 0 && x < grid(0).size