package solutions.Day17

import scala.annotation.tailrec

def solve(input: String): Unit =
    val (state, program) = parse(input)
    println(part1(state, program))
    println(part2(input.split("\n\n")(1).strip.split(": ")(1).split(",").map(_.toLong)))

def part1(state: ProgramState, program: Array[Instruction]): String =
    runProgram(state, program).output.reverse.mkString(",")

def part2(numbers: Array[Long]): Long =
    // there is one jump - as the last instruction, it jumps to the front
    // B and C are set based only on the value of few lowest bits of A at the start of every iteration
    // in every iteration A is divided by 8
    val program = parseProgram(numbers)
    (0 to numbers.length - 1).foldLeft(List(0L))((acc, i) =>
        acc.flatMap(ac => (for { a <- 0 to 7 } yield
            ((ac << 3) | a, runProgram(ProgramState(0, (ac << 3) | a, 0, 0, List.empty), program.init).output.head == numbers(numbers.length - 1 - i)))
        .filter(_._2)
        .map(_._1))).min

def parse(s: String): (ProgramState, Array[Instruction]) =
    val parts = s.split("\n\n")
    val regs = parts(0).split("\n").map(_.split(": ")(1))
    val state = ProgramState(0, regs(0).toLong, regs(1).toLong, regs(2).toLong, List.empty)
    val program = parseProgram(parts(1).split(": ")(1).strip.split(",").map(_.toLong))
    (state, program)

def parseProgram(program: Array[Long]): Array[Instruction] =
    program.grouped(2).map(a => parseInstruction(a(0), a(1))).toArray

def parseInstruction(instruction: Long, operand: Long): Instruction =
    instruction match
        case 0 => Instruction(Opcode.Adv, parseComboOperand(operand))
        case 1 => Instruction(Opcode.Bxl, Immediate(operand))
        case 2 => Instruction(Opcode.Bst, parseComboOperand(operand))
        case 3 => Instruction(Opcode.Jnz, Immediate(operand))
        case 4 => Instruction(Opcode.Bxc, Immediate(operand))
        case 5 => Instruction(Opcode.Out, parseComboOperand(operand))
        case 6 => Instruction(Opcode.Bdv, parseComboOperand(operand))
        case 7 => Instruction(Opcode.Cdv, parseComboOperand(operand))
        case _ => throw RuntimeException("Should never happen")

def parseComboOperand(operand: Long): Operand =
    operand match
        case 0 | 1 | 2 | 3 => Immediate(operand)
        case 4 => RegisterA()
        case 5 => RegisterB()
        case 6 => RegisterC()
        case _ => throw RuntimeException("Should never happen")

@tailrec def runProgram(state: ProgramState, program: Array[Instruction]): ProgramState =
    if state.pc >= program.length then
        state
    else
        val instruction = program(state.pc)
        val operand = evaluateOperand(state, instruction.operand)
        val newState = instruction.opcode match
            case Opcode.Adv => state.copy(pc = state.pc + 1, ra = state.ra >> operand)
            case Opcode.Bxl => state.copy(pc = state.pc + 1, rb = state.rb ^ operand)
            case Opcode.Bst => state.copy(pc = state.pc + 1, rb = operand & 7)
            case Opcode.Jnz => state.copy(pc = if state.ra == 0 then state.pc + 2 else (operand >> 1L).toInt)
            case Opcode.Bxc => state.copy(pc = state.pc + 1, rb = state.rb ^ state.rc)
            case Opcode.Out => state.copy(pc = state.pc + 1, output = (operand & 7).toInt :: state.output)
            case Opcode.Bdv => state.copy(pc = state.pc + 1, rb = state.ra / (1 << operand))
            case Opcode.Cdv => state.copy(pc = state.pc + 1, rc = state.ra / (1 << operand))
        runProgram(newState, program)

def evaluateOperand(state: ProgramState, operand: Operand): Long =
    operand match
        case Immediate(x) => x
        case RegisterA() => state.ra
        case RegisterB() => state.rb
        case RegisterC() => state.rc
    

case class ProgramState(val pc: Int, val ra: Long, val rb: Long, val rc: Long, val output: List[Int])

case class Instruction(val opcode: Opcode, val operand: Operand)

enum Opcode:
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

sealed trait Operand
case class Immediate(val x: Long) extends Operand
case class RegisterA() extends Operand
case class RegisterB() extends Operand
case class RegisterC() extends Operand