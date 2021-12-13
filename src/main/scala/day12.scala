package advent2021.day12
import utils.*

def run: Unit = {
    val g = graph
    val paths = g.allPaths(Set(List(g.start)), Set())
    println(f"part 1 ${paths.size}")
    val sillyPaths = g.allSillyPaths(Set(List(g.start)), Set())
    println(f"part 2 ${sillyPaths.size}")
}

def graph: Graph = {
    val allEdges: List[(String, String)] = utils.lines(12).map { r => 
        val Array(a,b) = r.split('-')
        (a, b)
    }
    Graph.fromEdgeList(allEdges)
}

object Graph {
    def empty: Graph = Graph(Set(), Map())
    def fromEdgeList(es: List[(String, String)]): Graph = {
        es.foldLeft(empty){(g, edge) =>
            val (a, b) = edge
            g + (Vertex.fromString(a), Vertex.fromString(b))
        }
    }
}

case class Graph(vs: Set[Vertex], ns: Map[Vertex, Set[Vertex]]) {
    def +(from: Vertex, to: Vertex): Graph = {
        val newVs = vs ++ Set(from, to)
        val newNs = ns + (from -> (ns.getOrElse(from, Set()) + to), to -> (ns.getOrElse(to, Set()) + from))
        Graph(newVs, newNs)
    }

    def start: Vertex = vs.find(_.name == "start").get
    def end: Vertex = vs.find(_.name == "end").get

    def allPaths(continuing: Set[List[Vertex]], arrived: Set[List[Vertex]]): Set[List[Vertex]] = {
        if continuing.isEmpty then
            arrived
        else
            val conts = continuing.flatMap(continuations(_))
            allPaths(conts.filter(_.head != end), arrived ++ conts.filter(_.head == end))
    }

    def continuations(path: List[Vertex]): Set[List[Vertex]] = {
        (ns(path.head) -- path.filter(! _.big)).map(_ :: path)
    }

    def allSillyPaths(continuing: Set[List[Vertex]], arrived: Set[List[Vertex]]): Set[List[Vertex]] = {
    if continuing.isEmpty then
        arrived
    else
        val conts = continuing.flatMap(sillyContinuations(_))
        allSillyPaths(conts.filter(_.head != end), arrived ++ conts.filter(_.head == end))
    }

    def sillyContinuations(path: List[Vertex]): Set[List[Vertex]] = {
        val seen = path.filter(! _.big)
        val hasDoubledUp = seen.length != seen.toSet.size
        if hasDoubledUp then
            (ns(path.head) -- path.filter(! _.big)).map(_ :: path)
        else
            (ns(path.head) - start).map(_ :: path)
    }
}

object Vertex {
    def fromString(str: String): Vertex = {
        Vertex(str, str.toList.head.isUpper)
    }
}

case class Vertex(name: String, big: Boolean)

