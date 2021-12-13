package advent2021.day8
import utils.*

def run: Unit = {
    println(f"part 1 ${part1}")
    println(f"part 2 ${part2}")
}

def part1 : Int = {
    utils.lines(8).map{ str =>
        val sstr = str.split(" \\| ")(1)
        sstr.trim.split(" ").map(_.length).count(List(2, 4, 3, 7).contains(_))
    }.sum
}

def part2 : BigInt = {
    utils.lines(8).map{str =>
        val Array(obs, value) = str.split(" \\| ")
        val legend: Map[Set[Char], Int] = generateLegend(obs.split(" ").map(_.toSet).toList)
        value.split(" ").map{ (str: String) => 
            legend.get(str.toSet).get
        }.foldLeft(BigInt(0))((acc, i) => 10 * acc + i)
    }.sum
}

def generateLegend(obs: List[Set[Char]]): Map[Set[Char], Int] = {
    val basic = Map(
        1 -> obs.find(_.size == 2).get,
        4 -> obs.find(_.size == 4).get,
        7 -> obs.find(_.size == 3).get,
        8 -> obs.find(_.size == 7).get
    )

    val sixes = obs.filter(_.size == 6)

    val im = basic ++ Map(
        0 -> sixes.find(s => has(basic, 1, s) && !has(basic, 4, s)).get,
        6 -> sixes.find(!has(basic, 1, _)).get,
        9 -> sixes.find(has(basic, 4, _)).get
    )

    val fives = obs.filter(_.size == 5)

    val adv = im ++ Map(
        5 -> fives.find(s => s.subsetOf(im.get(6).get)).get,
        3 -> fives.find(s => has(im, 1, s)).get,
        2 -> fives.find(s => !has(im, 1, s) && !s.subsetOf(im.get(6).get)).get
    )
    
    adv.foldLeft(Map[Set[Char], Int]())( (mp, pr) => mp + ((pr._2, pr._1)))
}

def has(m: Map[Int, Set[Char]], i: Int, s: Set[Char]): Boolean = {
    m.get(i).get.subsetOf(s)
}