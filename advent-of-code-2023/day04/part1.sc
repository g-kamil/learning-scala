import scala.util.Using
import scala.io.Source
import scala.math.pow

Using(scala.io.Source.fromFile("input.txt")) { source =>

  val lines = source.getLines().map(_.split(":")(1))
  var result = 0

  for (line <- lines) {
    val Array(winning, choosen) = line.replaceAll(" +", " ").split("\\|").map(_.trim).map(_.split(" "))

    val matching = winning.intersect(choosen).length

    result += pow(2, matching - 1).toInt
  }

  println(result)
}