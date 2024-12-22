package solutions.Day22

def solve(input: String): Unit =
    val numbers = input.split("\n").map(_.toLong)
    println(part1(numbers))

def part1(numbers: Array[Long]): Long =
    val m = transformationMatrix.pow(2000)
    numbers.map(a => vectorToInt(m * intToVector(a.toInt, 24)).toLong).sum

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