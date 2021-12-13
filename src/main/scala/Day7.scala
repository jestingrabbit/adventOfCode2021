package advent2021.day7
import utils.*
import scala.collection.immutable.Queue

def run: Unit = {
    val cs = crabs
    println(f"part 1 ${align(cs, 0)}")
    val scs = simpleCrabs
    println(f"part 2 ${trueFuel(scs, 1, fuelToAlignAt(scs, 0))}")

}

def simpleCrabs: Map[Int, Int] = {
    utils.lines(7).head.split(",").map(_.toInt).groupBy[Int](x => x).map((k, v) => (k, v.length))
}

def crabs: Queue[(Int, Int)] = {
    val positions: Array[Int] = utils.lines(7).head.split(",").map(_.toInt)
    val groupedSortedPositions: List[(Int, Int)] = positions.groupBy[Int](x => x).map((k, v) => (k, v.length)).toList.sorted
    Queue.from(groupedSortedPositions)
}

def align(cs: Queue[(Int, Int)], fuel: Int): Int = {
    if cs.length == 1 then
        fuel
    else
        val (fx, fp) = cs.head
        val (lx, lp) = cs.last

        if fp > lp then
            val Queue((slx, slp), (lx, lp)) = cs.takeRight(2)
            align(cs.dropRight(2) :+ (slx, slp + lp), fuel + lp * (lx - slx))
        else
            val Queue((fx, fp), (sx, sp)) = cs.take(2)
            align((sx, fp + sp) +: cs.drop(2), fuel + fp * (sx - fx))
}

def trueFuel(cs: Map[Int, Int], x: Int, oldCost: Int): Int = {
    val newFuel = fuelToAlignAt(cs, x)
    if newFuel < oldCost then
        trueFuel(cs, x + 1, newFuel)
    else
        oldCost
}

def fuelToAlignAt(cs: Map[Int, Int], x: Int): Int = {
    cs.map((k, v) =>  tri((k - x).abs) * v).sum
}

def tri(a: Int): Int = (a * (a + 1)) / 2