import solutions.Day09

class Day09 extends munit.FunSuite:
    test("Part 1 example") {
        val input = "2333133121414131402"
        val disk = (if input.size % 2 != 0 then input.toList else input.toList.init).map(_.toLong - '0'.toLong)
        val ids = (0L to disk.size / 2).toList
        assertEquals(Day09.part1(disk, ids), 1928L)
    }

    test("Part 2 example") {
        val input = "2333133121414131402"
        val disk = (if input.size % 2 != 0 then input.toList else input.toList.init).map(_.toLong - '0'.toLong)
        val elements = disk.scanLeft((0L, 0L))((state, elem) => (state._1 + state._2, elem)).tail
        val (filesList, spacesList) = elements.zipWithIndex.partition(_._2 % 2 == 0)
        val files = filesList.map(_._1).zipWithIndex.map((ps, id) => (id.toLong, Day09.File(ps._1, ps._2))).toMap
        val spaces = spacesList.map(_._1)
        assertEquals(Day09.part2(files, spaces, files.size - 1), 2858L)
    }