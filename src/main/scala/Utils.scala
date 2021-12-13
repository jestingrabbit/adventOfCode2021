package utils

import scala.io.Source

def lines(day: Int): List[String] = {
    val name = f"data/$day.txt"
    Source.fromFile(name).getLines.toList
}