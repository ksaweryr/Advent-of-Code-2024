package solutions.Day24

import scala.collection.{mutable => M}

def solve(input: String): Unit =
    val circuit = parseInput(input)
    println(part1(circuit))

def parseInput(input: String): Map[String, Gate] =
    val a = input.split("\n\n")
    parseInputs(a(0)) ++ parseGates(a(1))

def parseInputs(s: String): Map[String, Gate] =
    s.split("\n").map(row => {
        val a = row.split(": ")
        (a(0), Input(a(1).toInt == 1))
    }).toMap

def parseGates(s: String): Map[String, Gate] =
    s.split("\n").map(row => {
        val a = row.split(" ")
        val g = (p: String, q: String) => a(1) match {
            case "AND" => AndGate(p, q)
            case "OR" => OrGate(p, q)
            case "XOR" => XorGate(p, q)
            case x => throw RuntimeException(s"Invalid gate: ${x}")
        }

        (a(4), g(a(0), a(2)))
    }).toMap

def part1(circuit: Map[String, Gate]): Long =
    val mem = M.Map.empty[String, Boolean]
    circuit.keys.filter(_.startsWith("z")).toList.sorted.map(evaluate(circuit, _, mem)).foldRight(0L)((b, acc) => (acc << 1) | (if b then 1L else 0L))

def evaluate(circuit: Map[String, Gate], wire: String, mem: M.Map[String, Boolean] = M.Map.empty): Boolean =
    if !mem.contains(wire) then
        mem.update(wire, circuit(wire) match
            case AndGate(a, b) => evaluate(circuit, a, mem) && evaluate(circuit, b, mem)
            case OrGate(a, b) => evaluate(circuit, a, mem) || evaluate(circuit, b, mem)
            case XorGate(a, b) => evaluate(circuit, a, mem) != evaluate(circuit, b, mem)
            case Input(active) => active
        )

    mem(wire)

sealed trait Gate
case class AndGate(val a: String, val b: String) extends Gate
case class OrGate(val a: String, val b: String) extends Gate
case class XorGate(val a: String, val b: String) extends Gate
case class Input(val active: Boolean) extends Gate