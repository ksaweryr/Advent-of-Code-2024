package solutions.Day09

import scala.annotation.tailrec

def solve(input: String): Unit =
    val disk = (if input.size % 2 != 0 then input.toList else input.toList.init).map(_.toLong - '0'.toLong)
    val ids = (0L to disk.size / 2).toList
    println(part1(disk, ids))
    val elements = disk.scanLeft((0L, 0L))((state, elem) => (state._1 + state._2, elem)).tail
    val (filesList, spacesList) = elements.zipWithIndex.partition(_._2 % 2 == 0)
    val files = filesList.map(_._1).zipWithIndex.map((ps, id) => (id.toLong, File(ps._1, ps._2))).toMap
    val spaces = spacesList.map(_._1)
    println(part2(files, spaces, files.size - 1))

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

class File(val pos: Long, val size: Long)

@tailrec def part2(files: Map[Long, File], spaces: List[(Long, Long)], currentId: Long): Long =
    if currentId == 0 then
        files.map((id, f) => id * intervalSum(f.pos, f.pos + f.size - 1)).sum
    else
        val file = files(currentId)
        val spaceToUse = spaces.zipWithIndex.find((s, idx) => s._2 >= file.size && s._1 < file.pos)
        spaceToUse match {
            case None => part2(files, spaces, currentId - 1)
            case Some((p, idx)) =>
                val (spacePos, spaceSize) = p
                if spaceSize == file.size then
                    part2(files + ((currentId, File(spacePos, file.size))), deleteAtIndex(spaces, idx), currentId - 1)
                else
                    part2(files + ((currentId, File(spacePos, file.size))), replaceAtIndex(spaces, idx, ((spacePos + file.size, spaceSize - file.size))), currentId - 1)
        }

def intervalSum(from: Long, to: Long): Long =
    (from + to) * (to - from + 1) / 2L

def deleteAtIndex[T](a: List[T], idx: Int): List[T] =
    a.take(idx) ++ a.drop(idx + 1)

def replaceAtIndex[T](a: List[T], idx: Int, v: T): List[T] =
    a.take(idx) ++ (v :: a.drop(idx + 1))