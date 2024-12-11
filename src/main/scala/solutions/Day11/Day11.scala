package solutions.Day11

import scala.annotation.tailrec

def solve(input: String): Unit =
    val nums = input.strip().split(" ").map(_.toLong).toList
    val s = Solver()
    println(s.simulate(nums, 25))
    println(s.simulate(nums, 75))

class Solver:
    var dp: Map[(Long, Long), Long] = Map.empty

    def simulate(nums: List[Long], numSteps: Long): Long =
        nums.map(n => lookup((n, numSteps))).sum

    def lookup(k: (Long, Long)): Long =
        val (n, s) = k
        if s == 0 then
            1
        else
            if !dp.contains(k) then
                if n == 0 then
                    val res = lookup((1, s - 1))
                    dp += (k, res)
                else
                    val numDigits = math.log10(n).toLong + 1L
                    if numDigits % 2L == 0L then
                        val m = math.pow(10, numDigits / 2).toLong
                        val res = lookup((n % m, s - 1)) + lookup((n / m, s - 1))
                        dp += (k, res)
                    else
                        val res = lookup(n * 2024, s - 1)
                        dp += (k, res)
            dp(k)