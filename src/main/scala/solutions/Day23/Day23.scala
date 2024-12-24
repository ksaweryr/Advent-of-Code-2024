package solutions.Day23

import scala.collection.{mutable => M}

def solve(input: String): Unit =
    val g = parseGraph(input)
    println(part1(g))
    println(part2(g))

def part1(graph: Map[String, Set[String]]): Int =
    graph.toList
        .flatMap((a, s) => s.map(b => (a, b)))
        .map((a, b) => graph.keys.count(c => graph(c).contains(a) && graph(c).contains(b) && (a(0) == 't' || b(0) == 't' || c(0) == 't'))).sum / 6

def part2(graph: Map[String, Set[String]]): String =
    graph.keys.map(maxClique(graph, _)).maxBy(_.size).toList.sorted.mkString(",")

def maxClique(graph: Map[String, Set[String]], v: String): Set[String] =
    powerset(graph(v).toSeq).find(isClique(graph, _)).get + v

def isClique(graph: Map[String, Set[String]], clique: Set[String]): Boolean =
    clique.forall(a => clique.forall(b => a == b || graph(a).contains(b)))

def powerset[A](s: Seq[A]): IndexedSeq[Set[A]] =
    (0 to s.size).reverse.flatMap(s.combinations).map(_.toSet)

def parseGraph(input: String): Map[String, Set[String]] =
    constructGraph(parseEdges(input))

def parseEdges(input: String): Vector[(String, String)] =
    input.split("\n").map(row => {
        val parts = row.split("-")
        (parts(0), parts(1))
    }).toVector

def constructGraph(edges: Vector[(String, String)]): Map[String, Set[String]] =
    val m = M.Map.empty[String, M.Set[String]]
    for (a, b) <- edges do
        if !m.contains(a) then
            m.update(a, M.Set.empty[String])
        if !m.contains(b) then
            m.update(b, M.Set.empty[String])
        m(a).add(b)
        m(b).add(a)
    m.map((k, v) => (k, v.toSet)).toMap