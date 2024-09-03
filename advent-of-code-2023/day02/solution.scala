import scala.io.Source
import scala.util.matching.Regex

object Part1 {
  // only 12 red cubes, 13 green cubes, and 14 blue cubes
  def main(args: Array[String]): Unit = {

    var result = 0
    val rules: Map[String, Int] = Map[String, Int](
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )

    val fSource = Source.fromFile("input.txt")
    val pattern = new Regex("\\s*(\\d+)\\s*(red|green|blue)")

    for (rawLine <- fSource.getLines()) {

      val line = rawLine.split(":")(1)

      val gameValidity = line.split(";").map { singleGame =>
        val validityArray = singleGame.split(",").map(_.trim).map {
          case pattern(number, color) => rules(color) >= number.toInt
          case _ => false }
        validityArray.forall(identity)
      }

      if (gameValidity.forall(identity)) result += rawLine.split(":")(0).split(" ")(1).toInt
    }

    // for debugging
    // println(s"$isValid - ${rawLine.split(":")(0).split(" ")(1).toInt}: ${colorMap}")

    println(result)
    fSource.close()
  }
}



object Part2 {
  // only 12 red cubes, 13 green cubes, and 14 blue cubes
  def main(args: Array[String]): Unit = {

    var result = 0

    val fSource = Source.fromFile("input.txt")
    val pattern = new Regex("(\\d+) (red|green|blue)")

    for (rawLine <- fSource.getLines()) {

      val line = rawLine.split(":")(1)
      val colorMap = scala.collection.mutable.Map(
        "red" -> 1,
        "green" -> 1,
        "blue" -> 1
      )

      line.replace(";",",").trim().split(",").foreach { singleRoll =>
        singleRoll.trim() match {
          case pattern(number, color) => if (colorMap(color) < number.toInt) colorMap(color) = number.toInt
          case _ => ()
        }
      }

      // for debugging
      // println(s"${rawLine.split(":")(0).split(" ")(1).toInt}: ${colorMap} = ${colorMap.values.foldLeft(1)((x, y) => x * y)}")
      result += colorMap.values.foldLeft(1)((x, y) => x * y)

    }

    println(result)
    fSource.close()
  }
}