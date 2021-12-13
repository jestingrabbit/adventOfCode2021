package advent2021.day6

import utils.*

def run: Unit = {
    val pop = population
    val totalPop80 = Range(0, 80).foldLeft(pop)((acc, _) => acc.step).vals.sum
    val totalPop256 = Range(0, 256).foldLeft(pop)((acc, _) => acc.step).vals.sum
    println(f"part 1 ${totalPop80}")
    println(f"part 2 ${totalPop256}")
}

def population: Population = {
    val ls = utils.lines(6).head.split(",").map(_.toInt).toList
    Population.fromList(ls)
}

object Population {
    def fromList(ls: List[Int]): Population = {
        val vs = ls.foldLeft(Vector.fill(9)(BigInt(0)))((pp, i) => pp.updated(i, pp(i) + 1))
        Population(vs)
    }
}

case class Population(vals: Vector[BigInt]) {
    def step: Population = {
        val spawning = vals(0)
        val rolledVals = vals.tail.appended(spawning)
        Population(rolledVals.updated(6, rolledVals(6) + spawning))
    }
}
