package solutions.Day04

def solve(input: String): Unit =
    val grid = input.strip().split("\n").map(_.toArray)
    println(part1(grid))
    println(part2(grid))

def part1(grid: Array[Array[Char]]): Int =
    (for { y <- grid.indices; x <- grid(0).indices } yield xmasCount(grid, (x,y))).sum

def part2(grid: Array[Array[Char]]): Int =
    (for { y <- grid.indices; x <- grid(0).indices } yield (x,y)).count(hasXmas2(grid, _))

def xmasCount(grid: Array[Array[Char]], position: (Int, Int)): Int =
    val (x, y) = position
    val canHaveHorizontal = x + 3 < grid(0).length
    val canHaveVertical = y + 3 < grid.length

    val horizontal = if canHaveHorizontal then
        val word = grid(y).slice(x, x + 4).mkString
        if word == "XMAS" || word == "SAMX" then 1 else 0
    else 0

    val vertical = if canHaveVertical then
        val word = grid.slice(y, y + 4).map(_(x)).mkString
        if word == "XMAS" || word == "SAMX" then 1 else 0
    else 0

    val diagonalUp = if canHaveHorizontal && y - 3 >= 0 then
        val word = grid.slice(y - 3, y + 1).reverse.zipWithIndex.map((elem, idx) => elem(x + idx)).mkString
        if word == "XMAS" || word == "SAMX" then 1 else 0
    else 0

    val diagonalDown = if canHaveHorizontal && canHaveVertical then
        val word = grid.slice(y, y + 4).zipWithIndex.map((elem, idx) => elem(x + idx)).mkString
        if word == "XMAS" || word == "SAMX" then 1 else 0
    else 0

    horizontal + vertical + diagonalUp + diagonalDown

def hasXmas2(grid: Array[Array[Char]], position: (Int, Int)): Boolean =
    val (x, y) = position
    if x == 0 || x == grid(0).length - 1 || y == 0 || y == grid.length - 1 then
        false
    else
        grid(y)(x) == 'A'
            && ((grid(y-1)(x-1) == 'M' && grid(y+1)(x+1) == 'S') || (grid(y-1)(x-1) == 'S' && grid(y+1)(x+1) == 'M'))
            && ((grid(y-1)(x+1) == 'M' && grid(y+1)(x-1) == 'S') || (grid(y-1)(x+1) == 'S' && grid(y+1)(x-1) == 'M'))