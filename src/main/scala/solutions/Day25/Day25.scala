package solutions.Day25

def solve(input: String): Unit =
    val (locks, keys) = parseInput(input)
    println(part1(locks, keys))

def parseInput(input: String): (Vector[Lock], Vector[Key]) =
    val parts = input.split("\n\n")
    val (locks, keys) = parts.map(parsePart).partition(_.isLeft)
    (locks.map(_.left.get).toVector, keys.map(_.right.get).toVector)

def parsePart(part: String): Either[Lock, Key] =
    val rows = part.split("\n")
    val transposed = for x <- 0 to rows(0).length - 1 yield
        for y <- 0 to rows.length - 1 yield
            rows(y)(x)
    if transposed(0)(0) == '#' then
        Left(Lock(transposed.map(col => col.indexOf('.') - 1).toVector))
    else
        Right(Key(transposed.map(col => col.reverse.indexOf('.') - 1).toVector))

def part1(locks: Vector[Lock], keys: Vector[Key]): Int =
    (for lock <- locks; key <- keys yield
        fits(key, lock)).count(p => p)
        
def fits(key: Key, lock: Lock): Boolean =
    key.pins.zip(lock.pins).map(_ + _).forall(_ <= 5)

case class Lock(val pins: Vector[Int])
case class Key(val pins: Vector[Int])