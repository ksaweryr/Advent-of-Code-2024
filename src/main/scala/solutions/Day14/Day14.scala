package solutions.Day14

def solve(input: String): Unit =
    val robots = input.split("\n").map(parseRobot(_, (101, 103)))
    println(part1(robots))

def part1(robots: Array[Robot]): Int =
    robots
        .map(_.move(100))
        .groupBy(_.quadrant())
        .filterKeys(_.isDefined)
        .values
        .map(_.length)
        .fold(1)(_ * _)

def parseRobot(s: String, bounds: (Int, Int)): Robot =
    val parts = s.split(" ")
    Robot(parseVector(parts(0)), parseVector(parts(1)), bounds)

def parseVector(s: String): (Int, Int) =
    val ns = s.split("=")(1).split(",")
    (ns(0).toInt, ns(1).toInt)

enum Quadrant:
    case TL, TR, BL, BR

class Robot(val p: (Int, Int), val v: (Int, Int), val bounds: (Int, Int)):
    def move(steps: Int): Robot =
        val (maxX, maxY) = bounds
        val (x0, y0) = p
        val (vx, vy) = v
        Robot((mod(x0 + steps * vx, maxX), mod(y0 + steps * vy, maxY)), v, bounds)
    def quadrant(): Option[Quadrant] =
        val (x, y) = p
        val (mx, my) = bounds
        val left = x < mx / 2
        val right = x > mx / 2
        val top = y < my / 2
        val bottom = y > my / 2
        if left && top then
            Some(Quadrant.TL)
        else if left && bottom then
            Some(Quadrant.BL)
        else if right && top then
            Some(Quadrant.TR)
        else if right && bottom then
            Some(Quadrant.BR)
        else
            None
    override def toString(): String =
        s"Robot(p=${p}, v=${v}, bounds=${bounds})"

def mod(a: Int, m: Int): Int =
    ((a % m) + m) % m