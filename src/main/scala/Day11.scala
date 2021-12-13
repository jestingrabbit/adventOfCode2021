package advent2021.day11
import utils.*

def run: Unit = {
    val g = energyGrid
    val (_, totalFlashesIn100) = (1 to 100).foldLeft((g, 0)){(acc, _) => 
        val (gg, total) = acc
        val (newG, flashed) = gg.step
        (newG, total + flashed.size)
    }
    println(f"part 1 ${totalFlashesIn100}")

    println(f"part 2 ${g.synchronizeAt(0)}")
}

def energyGrid: Grid = {
    val narrs = utils.lines(11).toArray.map(_.split("").map(_.toInt))
    Grid.fromNestedArrays(narrs)
}

object Grid {
    def indices(h: Int, w: Int): Set[Coords] = {
        val cs = for { i <- 0 until w
                       j <- 0 until h } yield Coords(i, j)
        cs.toSet
    }

    def fromNestedArrays(arrs: Array[Array[Int]]): Grid = {
        val h = arrs.length
        val w = arrs(0).length
        
        Grid(h, w, Grid.indices(h, w).map(c => (c, arrs(c.y)(c.x))).toMap)
    }
}

case class Grid(h: Int, w: Int, vs:Map[Coords, Int]) {
    def indices: Set[Coords] = Grid.indices(h, w)

    def step: (Grid, Set[Coords]) = {
        val newVals = vs.map((k, v) => (k, v + 1))
        val firstFlashers = indices.filter(newVals(_) > 9)
        val (newGrid, flashers) = Grid(h, w, newVals).fullSpread(firstFlashers, Set())
        (Grid(h, w, newGrid.vs ++ flashers.map((_, 0)).toMap), flashers)
    }

    def fullSpread(flashing: Set[Coords], flashed: Set[Coords]): (Grid, Set[Coords]) = {
    if flashing.isEmpty then
        (this, flashed)
    else
        val newFlashed = flashing ++ flashed
        val newVals: Map[Coords, Int]= flashing.foldLeft(this.vs) { (vss, c) => 
            val updatedDNeighbours: Map[Coords, Int] = c.validDNeighbours(h, w).map(oc => (oc, vss(oc) + 1)).toMap
            vss ++ updatedDNeighbours
        }

        val newFlashing = (flashing.flatMap(_.validDNeighbours(h,w)) -- newFlashed).filter(newVals(_) > 9)
        Grid(h, w, newVals).fullSpread(newFlashing, newFlashed)
    }

    def synchronizeAt(n: Int): Int = {
        val (newGrid, flashers) = step
        if flashers.size == 100 then
            n + 1
        else
            newGrid.synchronizeAt(n + 1)
    }
}

object Coords {
    def fromPair(pr: (Int, Int)): Coords = Coords(pr._1, pr._2)
    def neighourDiffs: Set[Coords] = {
        Set((1, 0), (-1, 0), (0, 1), (0, -1)).map(fromPair)
    }

    def diagDiffs: Set[Coords] = {
        val cs = for { i <- -1 to 1
                       j <- -1 to 1 } yield Coords(i, j)
        cs.toSet - Coords(0, 0)
    }
}

case class Coords(x: Int, y: Int) {
    def +(other: Coords): Coords = {
        Coords(x + other.x, y + other.y)
    }

    def neighbours: Set[Coords] = Coords.neighourDiffs.map(_ + this)

    def dNeighbours: Set[Coords] = Coords.diagDiffs.map(_ + this)

    def validDNeighbours(h: Int, w: Int): Set[Coords] = dNeighbours.filter { c => 
        c.x >= 0 && c.y >= 0 && c.x < w && c.y < h
    }
}