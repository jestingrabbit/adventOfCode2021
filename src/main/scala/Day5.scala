package advent2021.day5

import utils.*

def run: Unit = {
    val ints = intervals   
    println(f"part 1 ${simpleOverlapsCount(ints, Set(), Set())}")
    println(f"part 2 ${complexOverlapsCount(ints, Set(), Set())}")
}

def complexOverlapsCount(intervals: List[Interval], lined: Set[Coords], overlaps: Set[Coords]): Int = {
    intervals match {
        case Nil => overlaps.size
        case iThis :: iRest => {
            val coords: Set[Coords] = iThis.toSet
            complexOverlapsCount(
                iRest,
                lined ++ coords,
                overlaps ++ (lined & coords)
            )
        }
    }
}

def simpleOverlapsCount(intervals: List[Interval], lined: Set[Coords], overlaps: Set[Coords]): Int = {
    intervals match {
        case Nil => overlaps.size
        case iThis :: iRest => {
            val coords: Set[Coords] = iThis.toSmallSet
            simpleOverlapsCount(
                iRest,
                lined ++ coords,
                overlaps ++ (lined & coords)
            )
        }
    }
}

def intervals: List[Interval] = {
    utils.lines(5).map(Interval.fromString).toList
}

object Interval {
    def fromString(str: String): Interval = {
        val Array(s, t) = str.split(" -> ").map(Coords.fromString)
        Interval(s, t)
    }
}

case class Interval(s: Coords, t: Coords) {
    def horizontal: Boolean = s.y == t.y
    def vertical: Boolean = s.x == t.x

    def toSmallSet: Set[Coords] = {
        if horizontal then
            (s.x to t.x by (t.x - s.x).sign).map(Coords(_, s.y)).toSet
        else if vertical then
            (s.y to t.y by (t.y - s.y).sign).map(Coords(s.x, _)).toSet
        else
            Set[Coords]()
    }

    def direction: Coords = Coords((t.x - s.x).sign, (t.y - s.y).sign)

    def toSet: Set[Coords] = {
        def build(from: Coords, to: Coords, direction: Coords, acc: Set[Coords]): Set[Coords] = {
            if from == to then acc + from
            else build(from + direction, to, direction, acc + from)
        }
        build(s, t, direction, Set())
    }
}

object Coords {
    def fromString(str: String) : Coords = {
      val Array(x, y) = str.split(",").map(_.toInt)
      Coords(x, y)
    }
}

case class Coords(x: Int, y: Int) {
    def +(other: Coords): Coords = {
        Coords(x + other.x, y + other.y)
    }
}