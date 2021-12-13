package advent2021.day10
import utils.*

def run: Unit = {
    val nls = navLines
    println(f"part 1 ${nls.map(errorCost(_, List())).sum}")
    val points: Array[BigInt] = navLines.flatMap(completionPoints(_, List())).toArray.sorted
    val l: Int = points.length
    println(f"part 2 ${points(l/2)}")
}

def navLines: List[List[Char]] = {
    utils.lines(10).map(_.toList)
    // List(
    //     "[({(<(())[]>[[{[]{<()<>>",
    //     "[(()[<>])]({[<{<<[]>>(",
    //     "{([(<{}[<>[]}>{[]{[(<()>",
    //     "(((({<>}<{<{<>}{[]{[]{}",
    //     "[[<[([]))<([[{}[[()]]]",
    //     "[{[{({}]{}}([{[{{{}}([]",
    //     "{<[[]]>}<{[{[{[]{()[[[]",
    //     "[<(<(<(<{}))><([]([]()",
    //     "<{([([[(<>()){}]>(<<{{",
    //     "<{([{{}}[<[[[<>{}]]]>[]]"
    // ).map(_.toList)
}

def errorCost(chars: List[Char], stack: List[Bracket]): Int = {
    chars match {
        case Nil => 0
        case a :: rest => {
            Bracket.classify(a) match {
                case (Some(b), _) => errorCost(rest, b :: stack)
                case (_, Some(c)) => {
                    stack match {
                        case d :: stackRest if c == d => errorCost(rest, stackRest)
                        case _ => c.cost
                    }
                }
            }
        } 
    } 
}

def completionPoints(chars: List[Char], stack: List[Bracket]): Option[BigInt] = {
    chars match {
        case Nil => Option(
            stack.foldLeft(BigInt(0))((acc, b) => 5 * acc + b.points)
        )
        case a :: rest => {
            Bracket.classify(a) match {
                case (Some(b), _) => completionPoints(rest, b :: stack)
                case (_, Some(c)) => {
                    stack match {
                        case d :: stackRest if c == d => completionPoints(rest, stackRest)
                        case _ => None
                    }
                }
            }
        } 
    }     
}

enum Bracket(val open: Char, val close: Char, val cost: Int, val points: Int) {
    case Round extends Bracket('(', ')', 3, 1)
    case Square extends Bracket('[', ']', 57, 2)
    case Curly extends Bracket('{', '}', 1197, 3)
    case Angled extends Bracket('<', '>', 25137, 4)
}

object Bracket {
    def classify(c: Char): (Option[Bracket], Option[Bracket]) = {
        (Bracket.opens.get(c), Bracket.closes.get(c))
    }
        
    def opens: Map[Char, Bracket] = values.map(b => (b.open, b)).toMap
    def closes: Map[Char, Bracket] = values.map(b => (b.close, b)).toMap
}