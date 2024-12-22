package solutions.Day22

def solve(input: String): Unit =
    val numbers = input.split("\n").map(_.toInt)
    println(part1(numbers))
    println(part2(numbers))

def part1(numbers: Array[Int]): Long =
    // kinda overengineered, I expected part 2 to be the same thing, but with taking like 2000000th number into account
    val m = transformationMatrix.pow(2000)
    numbers.map(a => vectorToInt(m * intToVector(a, 24)).toLong).sum

def part2(numbers: Array[Int]): Long =
    val maps = numbers.map(part2SingleNumber)
    val keys = maps.map(_.keySet).flatten.toSet
    keys.map(k => maps.map(_.getOrElse(k, 0).toLong).sum).max

def part2SingleNumber(n: Int): Map[(Int, Int, Int, Int), Int] =
    val numbers = Stream.unfold(n)(s => Some((s % 10, nextNumber(s)))).take(2001).toList
    val differences = numbers.zip(numbers.tail).map((a, b) => (b - a)).toList
    val differencesQuadruples = differences.sliding(4).map(a => (a(0), a(1), a(2), a(3))).toList
    val numbersWithKeys = differencesQuadruples.zip(numbers.drop(4)).toList
    numbersWithKeys.foldRight(Map.empty[(Int, Int, Int, Int), Int])((kv, m) => m.updated(kv._1, kv._2))

def nextNumber(n: Int): Int =
    val m = (1 << 24) - 1
    val a = (n ^ (n << 6)) & m
    val b = (a ^ (a >> 5)) & m
    val c = (b ^ (b << 11)) & m
    c

case class GF2Matrix(val values: Vector[Vector[Int]]):
    def transpose(): GF2Matrix =
        GF2Matrix((for j <- 0 to values(0).length - 1 yield
            (for i <- 0 to values.length - 1 yield values(i)(j)).toVector).toVector)

    def row(idx: Int): Vector[Int] =
        values(idx)

    def col(idx: Int): Vector[Int] =
        (for i <- 0 to values.length - 1 yield values(i)(idx)).toVector

    def *(rhs: GF2Matrix): GF2Matrix =
        assert(values(0).length == rhs.values.length)
        GF2Matrix((for y <- 0 to values.length - 1 yield
            (for x <- 0 to rhs.values(0).length - 1 yield
                row(y).zip(rhs.col(x)).map(_ * _).sum & 1).toVector).toVector)

    def +(rhs: GF2Matrix): GF2Matrix =
        assert(values.length == rhs.values.length && values(0).length == rhs.values(0).length)
        GF2Matrix(values.zip(rhs.values).map((r1, r2) => r1.zip(r2).map(_ ^ _)))

    def pow(exp: Int): GF2Matrix =
        assert(values.length == values(0).length)
        if exp == 0 then
            shiftedIdentityMatrix(values.length)
        else if exp == 1 then
            this
        else if exp % 2 == 0 then
            val b = pow(exp / 2)
            b * b
        else
            this * pow(exp - 1)

def intToVector(a: Int, numbits: Int): GF2Matrix =
    GF2Matrix(Vector((0 to numbits - 1).reverse.map(b => (a >> b) & 1).toVector)).transpose()

def vectorToInt(v: GF2Matrix): Int =
    assert(v.values(0).length == 1)
    v.values.flatten.fold(0)((acc, b) => (acc << 1) | b)

def shiftedIdentityMatrix(n: Int, shift: Int = 0): GF2Matrix =
    GF2Matrix((0 to n - 1).map(y => (0 to n - 1).map(x => if x == y + shift then 1 else 0).toVector).toVector)

val stage1Matrix = shiftedIdentityMatrix(24) + shiftedIdentityMatrix(24, 6)
val stage2Matrix = shiftedIdentityMatrix(24) + shiftedIdentityMatrix(24, -5)
val stage3Matrix = shiftedIdentityMatrix(24) + shiftedIdentityMatrix(24, 11)
val transformationMatrix = stage3Matrix * stage2Matrix * stage1Matrix