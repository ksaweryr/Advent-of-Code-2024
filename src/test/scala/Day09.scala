import solutions.Day09

class Day09 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "2333133121414131402"
        val disk = (if input.size % 2 != 0 then input.toList else input.toList.init).map(_.toLong - '0'.toLong)
        val ids = (0L to disk.size / 2).toList
        assertEquals(Day09.part1(disk, ids), 1928L)
    }