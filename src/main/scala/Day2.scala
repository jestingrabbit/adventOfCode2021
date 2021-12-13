package advent2021.day2

import utils.*

def run: Unit = {
    val (a: Int, b: Int) = destination
    println(f"part 1 ${a*b}")
    val AimedPos(aim, x, y) = trueDestination
    println(f"part 2 ${x*y}")
}

def destination: (Int, Int) = {
    pairs.foldLeft((0,0))((p1, p2) => (p1._1 + p2._1, p1._2 + p2._2))
}

def trueDestination: AimedPos = {
    instructions.foldLeft(AimedPos(0, 0, 0))((pos, inst) => pos.doInstruction(inst))
} 

def pairs: List[(Int, Int)] = {
    utils.lines(2).map(lineToPair)
}

def lineToPair(str: String): (Int, Int) = {
    str.split(" ") match {
        case Array(direction, magnitude) => {
            val n: Int = magnitude.toInt
            direction match {
                case "forward" => (n, 0)
                case "up" => (0, -n)
                case "down" => (0, n)
            }
        }
    }
}

def instructions: List[Instruction] = {
    utils.lines(2).map(Instruction.fromString)
}

sealed trait Instruction

case class Forward(n: Int) extends Instruction
case class Up(n: Int) extends Instruction
case class Down(n: Int) extends Instruction

object Instruction {
    def fromString(str: String) : Instruction = {
        str.split(" ") match {
            case Array(direction, magnitude) => {
                val n: Int = magnitude.toInt
                direction match {
                    case "forward" => Forward(n)
                    case "up" => Up(n)
                    case "down" => Down(n)
                }
            }
        }
    }     
}

case class AimedPos(aim: BigInt, x: BigInt, y: BigInt) {
  def doInstruction(i: Instruction): AimedPos = {
      i match {
          case Forward(n) => AimedPos(aim, x + n, y + aim * n)
          case Up(n) => AimedPos(aim - n, x, y)
          case Down(n) => AimedPos(aim + n, x, y)
      }
  }
}