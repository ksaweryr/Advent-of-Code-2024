package solutions.Day17

import scala.annotation.tailrec

def solve(input: String): Unit =
    val (state, program) = parse(input)
    println(part1(state, program))

def part1(state: ProgramState, program: Array[Instruction]): String =
    runProgram(state, program).output.reverse.mkString(",")

def parse(s: String): (ProgramState, Array[Instruction]) =
    val parts = s.split("\n\n")
    val regs = parts(0).split("\n").map(_.split(": ")(1))
    val state = ProgramState(0, regs(0).toInt, regs(1).toInt, regs(2).toInt, List.empty)
    val program = parseProgram(parts(1).split(": ")(1).strip.split(",").map(_.toInt))
    (state, program)

def parseProgram(program: Array[Int]): Array[Instruction] =
    program.grouped(2).map(a => parseInstruction(a(0), a(1))).toArray

def parseInstruction(instruction: Int, operand: Int): Instruction =
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

def parseComboOperand(operand: Int): Operand =
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
            case Opcode.Adv => state.copy(pc = state.pc + 1, ra = state.ra / (1 << operand))
            case Opcode.Bxl => state.copy(pc = state.pc + 1, rb = state.rb ^ operand)
            case Opcode.Bst => state.copy(pc = state.pc + 1, rb = operand & 7)
            case Opcode.Jnz => state.copy(pc = if state.ra == 0 then state.pc + 2 else operand / 2)
            case Opcode.Bxc => state.copy(pc = state.pc + 1, rb = state.rb ^ state.rc)
            case Opcode.Out => state.copy(pc = state.pc + 1, output = (operand & 7) :: state.output)
            case Opcode.Bdv => state.copy(pc = state.pc + 1, rb = state.ra / (1 << operand))
            case Opcode.Cdv => state.copy(pc = state.pc + 1, rc = state.ra / (1 << operand))
        runProgram(newState, program)

def evaluateOperand(state: ProgramState, operand: Operand): Int =
    operand match
        case Immediate(x) => x
        case RegisterA() => state.ra
        case RegisterB() => state.rb
        case RegisterC() => state.rc
    

case class ProgramState(val pc: Int, val ra: Int, val rb: Int, val rc: Int, val output: List[Int])

case class Instruction(val opcode: Opcode, val operand: Operand)

enum Opcode:
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

sealed trait Operand
case class Immediate(val x: Int) extends Operand
case class RegisterA() extends Operand
case class RegisterB() extends Operand
case class RegisterC() extends Operand