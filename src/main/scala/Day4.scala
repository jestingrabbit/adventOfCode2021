package advent2021.day4

import utils.*

def run: Unit = {
    val (calls, boards) = callsAndBoards
   
    println(f"part 1 ${findWinningScore(calls, boards)}")
    println(f"part 2 ${findLastWinningScore(calls, boards)}")

}

def findLastWinningScore(calls: List[Int], boards: List[Board]): Int = {
    calls match {
        case i :: rest => {
            val boardsAfterCall = boards.map(_.mark(i))
            boardsAfterCall.filter(! _.won) match {
                case Nil => boardsAfterCall.head.score(i)
                case losers => findLastWinningScore(rest, losers)
            }
        } 
    }
}

def findWinningScore(calls: List[Int], boards: List[Board]): Int = {
    calls match {
        case i :: rest => {
            val newBoards = boards.map(_.mark(i))
            newBoards.filter(_.won) match {
                case Nil => findWinningScore(rest, newBoards)
                case winner :: _ =>  winner.score(i)
            }
         } 
    }
}

def callsAndBoards: (List[Int], List[Board]) = {
    val lines = utils.lines(4)
    val calls = lines.head.split(",").map(_.toInt).toList
    val boards = lines.tail.grouped(6).map(Board.fromList(_)).toList
    (calls, boards)
}

object Board {
    def fromList(ls: List[String]) = {
        val grid: List[Array[Int]] = ls.tail.map(_.trim.split("\\s+").map(_.toInt))
        val numbers: Map[Int, (Int, Int)] = grid.zipWithIndex.foldLeft(Map[Int, (Int, Int)]())((acc, nsi) =>
            val (ns, i) = nsi
            ns.zipWithIndex.foldLeft(acc)((accc, nj) =>
                val (n, j) = nj
                accc + (n -> (i, j))
            )
        )
        Board(numbers, numbers.keySet, Set(), false)
    }
}

case class Board(numbers: Map[Int, (Int, Int)], unmarked: Set[Int], seen: Set[(Int, Int)], won: Boolean) {
    def mark(i: Int): Board = {
        if numbers.contains(i) then
            val (x, y) = numbers(i)
            val wonNow = won || (seen.filter(_._1 == x).size == 4) || (seen.filter(_._2 == y).size == 4)
            Board(numbers, unmarked - i, seen + ((x, y)), wonNow)
        else
            this
    }

    def score(i: Int): Int = unmarked.sum * i
}

