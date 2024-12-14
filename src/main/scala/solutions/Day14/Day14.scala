package solutions.Day14

import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

def solve(input: String): Unit =
    val robots = input.split("\n").map(parseRobot(_, (101, 103)))
    println(part1(robots))
    part2(robots)

def part1(robots: Array[Robot]): Int =
    robots
        .map(_.move(100))
        .groupBy(_.quadrant())
        .filterKeys(_.isDefined)
        .values
        .map(_.length)
        .fold(1)(_ * _)

def part2(robots: Array[Robot]): Unit =
    Seq.unfold((0, robots))((step, robots) => {
        val im = draw(robots, (210, 210))
        val f = File(f"output/day14/step_${step}%04d.png")
        f.mkdirs()
        ImageIO.write(im, "png", f)
        if step <= 9999 then
            Some(((), (step + 1, robots.map(_.move(1)))))
        else
            None
    })

def draw(robots: Array[Robot], size: (Int, Int)): BufferedImage =
    val im = BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    val g = im.createGraphics()
    g.setBackground(Color.BLACK)
    g.setPaint(Color.GREEN)

    val cellSize = (size._1 / 101).min(size._2 / 103)
    val drawnWidth = cellSize * 101
    val drawnHeight = cellSize * 103
    val offsetX = (size._1 - drawnWidth) / 2
    val offsetY = (size._2 - drawnHeight) / 2

    for r <- robots do
        g.fillRect(offsetX + r.p._1 * cellSize, offsetY + r.p._2 * cellSize, cellSize, cellSize)
    
    g.dispose()

    im

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