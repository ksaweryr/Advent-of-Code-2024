package solutions.Day21

def solve(input: String): Unit =
    val codes = input.split("\n")
    println(part1(codes))
    println(part2(codes))

val part1 = solveForAllCodes(2)
val part2 = solveForAllCodes(25)

def solveForAllCodes(numIterations: Int)(codes: Array[String]): Long =
    codes.map(solveForSingleCode(numIterations)).sum

def solveForSingleCode(numIterations: Int)(code: String): Long =
    val numericPart = code.init.toLong
    val dp = scala.collection.mutable.Map.empty[(Char, Char, Int), Long]
    val seq = numericToHigherLevel(code)
    numericPart * ('A' + seq).zip(seq).map((a, b) => shortestPathAtLevel(a, b, numIterations, dp)).sum

def shortestPathAtLevel(from: Char, to: Char, level: Int, dp: scala.collection.mutable.Map[(Char, Char, Int), Long]): Long =
    if level == 0 then
        return 1L

    if !dp.contains((from, to, level)) then
        val path = directionalShortestPath(from, to)
        val cost = ('A' + path).zip(path).map((a, b) => shortestPathAtLevel(a, b, level - 1, dp)).sum
        dp.update((from, to, level), cost)
    dp((from, to, level))

def numericToHigherLevel(code: String, prevChar: Char = 'A'): String =
    if code.length == 0 then
        ""
    else
        numericShortestPath(prevChar, code.head) + numericToHigherLevel(code.tail, code.head)

def numericShortestPath(from: Char, to: Char): String =
    val (dx, dy) = distanceBetweenKeys(from, to, numericKeysLocations)
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
        val horADist = distanceBetweenKeys(hor(0), 'A', directionalKeysLocations)
        val verADist = distanceBetweenKeys(ver(0), 'A', directionalKeysLocations)
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
    val (dx, dy) = distanceBetweenKeys(from, to, directionalKeysLocations)
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
        val horADist = distanceBetweenKeys(hor(0), 'A', directionalKeysLocations)
        val verADist = distanceBetweenKeys(ver(0), 'A', directionalKeysLocations)
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

def distanceBetweenKeys(from: Char, to: Char, movementMap: Map[Char, (Int, Int)]): (Int, Int) =
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