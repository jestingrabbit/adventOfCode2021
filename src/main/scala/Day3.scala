package advent2021.day3

import utils.*

def run: Unit = {
    val (g: BigInt, e: BigInt) = gammaAndEpsilon
    println(f"part 1 ${g*e}")
    println(f"part 2 ${scrubberRating * oxyRating}")

}

def gammaAndEpsilon: (BigInt, BigInt) = {
    val (count: Int, posCounts: Array[Int]) =   tallies
    val cutoff = count/2
    val gamma = binToValue(posCounts.map(x => if x > cutoff then 1 else 0))
    val epsilon = binToValue(posCounts.map(x => if x < cutoff then 1 else 0))
    (gamma, epsilon)
}

def tallies: (Int, Array[Int]) = {
    val rs = readings
    readings.foldLeft((0, rs.head))((tally, reading) => 
        val (count, posCounts) = tally
        (count + 1, posCounts.zip(reading).map(_ + _))
    )
}

def scrubberRating: BigInt = binToValue(valueFinder(0, readings, scrubberCond))
def oxyRating: BigInt = binToValue(valueFinder(0, readings, oxyCond))

def scrubberCond(i: Int, count: Int, count1s: Int): (Array[Int] => Boolean) = {
    ar => ((2 * count1s < count) ==  (ar(i) == 1))
}

def oxyCond(i: Int, count: Int, count1s: Int): (Array[Int] => Boolean) = {
    ar => (2 * count1s >= count) ==  (ar(i) == 1)
}

def valueFinder(i: Int, candidates: List[Array[Int]], condition: (Int, Int, Int) => (Array[Int] => Boolean)): Array[Int] = {
    if (candidates.length == 1)
        candidates.head
    else
        val (count, count1s) = candidates.foldLeft((0, 0))((acc, ar) => {
          val (count, count1s) = acc
          (count + 1, count1s + (if (ar(i)== 1) then 1 else 0))
        })
        valueFinder(i + 1, candidates.filter(condition(i, count, count1s)), condition)
}

def readings: List[Array[Int]] = {
  utils.lines(3).map(str => str.split("").map(c => if (c == "1") then 1 else 0))
}

def binToValue(bs: Array[Int]): BigInt = {
    bs.foldLeft(BigInt(0))(2 * _ + _)
}