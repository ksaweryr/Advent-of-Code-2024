package solutions.Day05

def solve(input: String): Unit =
    val Array(a, b) = input.split("\n\n")
    val rules = a.split("\n").map(parseRule)
    val updates = b.split("\n").map(parseUpdate)
    println(part1(rules, updates))
    println(part2(rules, updates))

def parseRule(s: String): (Int, Int) =
    val Array(a, b) = s.split("\\|")
    (a.toInt, b.toInt)

def parseUpdate(s: String): Array[Int] =
    s.split(",").map(_.toInt)

def part1(rules: Array[(Int, Int)], updates: Array[Array[Int]]): Int =
    updates.filter(isSorted(_, rules)).map(u => u(u.size / 2)).sum

def part2(rules: Array[(Int, Int)], updates: Array[Array[Int]]): Int =
    updates.filterNot(isSorted(_, rules)).map(topoSort(_, rules)).map(u => u(u.size / 2)).sum

def isSorted(update: Array[Int], rules: Array[(Int, Int)]): Boolean =
    val filteredRules = rules.filter((a, b) => update.contains(a) && update.contains(b))
    val successors = filteredRules.groupBy(_._1).map((k, v) => (k, v.map(_._2)))
    var predecessorCounts = filteredRules.groupBy(_._2).map((k, v) => (k, v.size)).to(collection.mutable.Map)
    (for { page <- update }
        yield if predecessorCounts.lift(page).getOrElse(0) != 0 then
            false
        else
            for { suc <- successors.lift(page).getOrElse(Array[Int]()) } do
                if predecessorCounts.contains(suc) then
                    predecessorCounts(suc) = predecessorCounts(suc) - 1
            true).forall(b => b)

def topoSort(update: Array[Int], rules: Array[(Int, Int)]): Array[Int] =
    val filteredRules = rules.filter((a, b) => update.contains(a) && update.contains(b))
    val successors = filteredRules.groupBy(_._1).map((k, v) => (k, v.map(_._2)))
    var predecessorCounts = filteredRules.groupBy(_._2).map((k, v) => (k, v.size)).to(collection.mutable.Map)

    var sorted = collection.mutable.ArrayBuffer.empty[Int]
    var ready = collection.mutable.Queue.from(update.filter(predecessorCounts.get(_).getOrElse(0) == 0))

    while ready.size > 0 do
        val page = ready.dequeue
        sorted += page
        for { suc <- successors.lift(page).getOrElse(Array[Int]()) } do
            if predecessorCounts.contains(suc) then
                predecessorCounts(suc) = predecessorCounts(suc) - 1
                if predecessorCounts(suc) == 0 then
                    ready.enqueue(suc)
    
    sorted.toArray