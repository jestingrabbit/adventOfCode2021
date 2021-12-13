package advent2021.day9
import utils.*

def run: Unit = {
    val g = depthGrid
    println(f"part 1 ${g.totalRiskLevel}")
    val part2: Int = g.lowPoints.map(g.basinSizeFor(_)).sorted(Ordering.Int.reverse).take(3).reduce(_ * _)
    println(f"part 2 ${part2}")
}

def depthGrid: Grid = {
    val narrs = utils.lines(9).toArray.map(_.split("").map(_.toInt))
    Grid.fromNestedArrays(narrs)
}

object Grid {
    def indices(h: Int, w: Int): Seq[Coords] = {
        val cs = for { i <- 0 until w
                       j <- 0 until h } yield Coords(i, j)
        cs.toSeq
    }

    def fromNestedArrays(arrs: Array[Array[Int]]): Grid = {
        val h = arrs.length
        val w = arrs(0).length
        
        Grid(h, w, Grid.indices(h, w).map(c => (c, arrs(c.y)(c.x))).toMap)
    }
}

case class Grid(h: Int, w: Int, vs:Map[Coords, Int]) {
    def indices: Seq[Coords] = Grid.indices(h, w)

    def riskLevel(c: Coords): BigInt = {
        val depth = vs.get(c).get
        val neighbourDepths: Set[Int] = c.neighbours.flatMap(vs.get(_))
        if neighbourDepths.filter(_ <= depth).isEmpty then
            depth + 1
        else
            0
    }

    def totalRiskLevel: BigInt = {
        indices.map(riskLevel).sum
    }

    def lowPoints: Seq[Coords] = {
        indices.filter(riskLevel(_) > 0)
    }

    def neighboursInBasin(c: Coords): Set[Coords] = {
        c.neighbours.filter { oc => 
            val v: Int = vs.getOrElse(oc, -1)
            (v >= 0) && (v < 9)
        }
    }

    def basin(edge: Set[Coords], members: Set[Coords]): Set[Coords] = {
        if edge.isEmpty then
            members
        else
            val newMembers = edge ++ members
            val newEdge = edge.flatMap(neighboursInBasin) -- newMembers
            basin(newEdge, newMembers)
    }

    def basinSizeFor(c: Coords) = {
        basin(Set(c), Set()).size
    }
}

object Coords {
    def fromPair(pr: (Int, Int)): Coords = Coords(pr._1, pr._2)
    def neighourDiffs: Set[Coords] = {
        Set((1, 0), (-1, 0), (0, 1), (0, -1)).map(fromPair)
    }
}

case class Coords(x: Int, y: Int) {
    def +(other: Coords): Coords = {
        Coords(x + other.x, y + other.y)
    }

    def neighbours: Set[Coords] = Coords.neighourDiffs.map(_ + this)
}