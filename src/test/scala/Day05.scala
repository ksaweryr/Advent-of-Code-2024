import solutions.Day05

class Day05 extends munit.FunSuite:
    test("Topological sort example") {
        val rules = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13"
            .split("\n").map(Day05.parseRule)
        val update = Array(75,97,47,61,53)
        assertEquals(Day05.topoSort(update, rules).toList, List(97,75,47,61,53))
    }