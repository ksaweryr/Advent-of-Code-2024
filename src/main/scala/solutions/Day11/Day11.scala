package solutions.Day11

import scala.annotation.tailrec

def solve(input: String): Unit =
    val nums = input.strip().split(" ").map(_.toLong).toList
    println(part1(nums))

@tailrec def part1(nums: List[Long], cnt: Int = 25): Long =
    if cnt == 0 then
        nums.size
    else
        part1(step(nums), cnt - 1)

@tailrec def step(nums: List[Long], acc: List[Long] = List.empty): List[Long] =
    nums match
        case head :: next => (if head == 0L then
            step(next, 1 :: acc)
        else
            val numDigits = math.log10(head).toLong + 1L
            if numDigits % 2L == 0L then
                val m = math.pow(10, numDigits / 2).toLong
                step(next, head % m :: head / m :: acc)
            else
                step(next, head * 2024 :: acc))
        case Nil => acc.reverse
    