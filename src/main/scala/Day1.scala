package advent2021.day1

import scala.io.Source

def run: Unit = {
    val incs = increases(depths, 0)
    println(f"part 1 $incs")
    val incs2 = increases2(depths, 0)
    println(f"part 2 $incs2")
}

def increases(depths: List[Int], tally: BigInt): BigInt = {
    depths match {
      case Nil => tally
      case a :: Nil => tally
      case a :: b :: rest => increases(b :: rest, gtCounter(a,b) + tally)
    }
}

def increases2(depths: List[Int], tally: BigInt): BigInt = {
    depths match {
      case a :: b :: c :: d :: rest => increases2(b :: c :: d :: rest, gtCounter(a,d) + tally)
      case _ => tally
    }
}

def depths : List[Int] = {
  Source.fromFile("data/1.txt").getLines().toList.map(_.toInt)
}

def gtCounter(a: Int, b: Int): BigInt = {
    if (b > a)
      1
    else
      0

} 