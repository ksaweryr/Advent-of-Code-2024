package solutions.Day21

def solve(input: String): Unit =
    val codes = input.split("\n")
    println(part1(codes))

def part1(codes: Array[String]): Int =
    codes.map(part1SingleCode).sum

def part1SingleCode(code: String): Int =
    val numericPart = code.init.toInt
    numericPart * directionalToHigherLevel(directionalToHigherLevel(numericToHigherLevel(code.toStream))).length

def numericToHigherLevel(code: Stream[Char], prevChar: Char = 'A'): Stream[Char] =
    code match
        case (head #:: tail) =>
            lazy val res = numericShortestPath(prevChar, head).toStream ++ numericToHigherLevel(tail, head)
            res
        case Stream.Empty => Stream.empty[Char]

def directionalToHigherLevel(keys: Stream[Char], prevChar: Char = 'A'): Stream[Char] =
    keys match
        case (head #:: tail) =>
            lazy val res = directionalShortestPath(prevChar, head).toStream ++ directionalToHigherLevel(tail, head)
            res
        case Stream.Empty => Stream.empty[Char]

def numericShortestPath(from: Char, to: Char): String =
    val (dx, dy) = shortestPath(from, to, numericKeysLocations)
    val hor = if dx > 0 then ">" * dx else "<" * -dx
    val ver = if dy > 0 then "v" * dy else "^" * -dy
    val horFirstLegal = {
        val (x, y) = numericKeysLocations(from)
        dx != 0 && (y != 3 || x + dx != 0)
    }
    val verFirstLegal = {
        val (x, y) = numericKeysLocations(from)
        dy != 0 && (x != 0 || y + dy != 3)
    }
    if horFirstLegal && verFirstLegal then
        val horADist = shortestPath(hor(0), 'A', directionalKeysLocations)
        val verADist = shortestPath(ver(0), 'A', directionalKeysLocations)
        val horFirstCost = verADist._1.abs + verADist._2.abs
        val verFirstCost = horADist._1.abs + horADist._2.abs
        if horFirstCost < verFirstCost then
            hor + ver + 'A'
        else
            ver + hor + 'A'
    else if horFirstLegal then
        hor + ver + 'A'
    else
        ver + hor + 'A'

def directionalShortestPath(from: Char, to: Char): String =
    val (dx, dy) = shortestPath(from, to, directionalKeysLocations)
    val hor = if dx > 0 then ">" * dx else "<" * -dx
    val ver = if dy > 0 then "v" * dy else "^" * -dy
    val horFirstLegal = {
        val (x, y) = directionalKeysLocations(from)
        dx != 0 && (y != 0 || x + dx != 0)
    }
    val verFirstLegal = {
        val (x, y) = directionalKeysLocations(from)
        dy != 0 && (x != 0 || y + dy != 0)
    }
    if horFirstLegal && verFirstLegal then
        val horADist = shortestPath(hor(0), 'A', directionalKeysLocations)
        val verADist = shortestPath(ver(0), 'A', directionalKeysLocations)
        val horFirstCost = verADist._1.abs + verADist._2.abs
        val verFirstCost = horADist._1.abs + horADist._2.abs
        if horFirstCost < verFirstCost then
            hor + ver + 'A'
        else
            ver + hor + 'A'
    else if horFirstLegal then
        hor + ver + 'A'
    else
        ver + hor + 'A'

def shortestPath(from: Char, to: Char, movementMap: Map[Char, (Int, Int)]): (Int, Int) =
    val (x0, y0) = movementMap(from)
    val (x1, y1) = movementMap(to)
    val dx = x1 - x0
    val dy = y1 - y0
    (dx, dy)

val numericKeysLocations = Map(
    ('A', (2, 3)),
    ('0', (1, 3)),
    ('1', (0, 2)),
    ('2', (1, 2)),
    ('3', (2, 2)),
    ('4', (0, 1)),
    ('5', (1, 1)),
    ('6', (2, 1)),
    ('7', (0, 0)),
    ('8', (1, 0)),
    ('9', (2, 0))
)

val directionalKeysLocations = Map(
    ('A', (2, 0)),
    ('^', (1, 0)),
    ('<', (0, 1)),
    ('v', (1, 1)),
    ('>', (2, 1))
)