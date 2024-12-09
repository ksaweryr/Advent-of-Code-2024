package solutions.Day09

import scala.annotation.tailrec

def solve(input: String): Unit =
    val disk = (if input.size % 2 != 0 then input.toList else input.toList.init).map(_.toLong - '0'.toLong)
    val ids = (0L to disk.size / 2).toList
    println(part1(disk, ids))

@tailrec def part1(disk: List[Long], ids: List[Long], position: Long = 0L, isCurrentlySpace: Boolean = false, acc: Long = 0L): Long =
    disk match {
        case head :: next =>
            if isCurrentlySpace then
                val trailingFileSize = disk.last
                if trailingFileSize == head then
                    part1(next.init.init, ids.init, position + head, false, acc + ids.last * intervalSum(position, position + head - 1))
                else if trailingFileSize < head then
                    part1((head - trailingFileSize) :: next.init.init, ids.init, position + trailingFileSize, true, acc + ids.last * intervalSum(position, position + trailingFileSize - 1))
                else // trailingFileSize > head
                    part1(next.init ++ List(trailingFileSize - head), ids, position + head, false, acc + ids.last * intervalSum(position, position + head - 1))
            else
                part1(next, ids.tail, position + head, true, acc + ids.head * intervalSum(position, position + head - 1))
        case Nil => acc
    }

def intervalSum(from: Long, to: Long): Long =
    (from + to) * (to - from + 1) / 2L