import scala.io.Source
import scala.util.matching.Regex

object Common {
  val pattern = new Regex("\\d")

  def getFirstAndOrLast(line: String): Int = {
    val list = Common.pattern.findAllIn(line).toList
    list match {
      case first :: Nil => s"$first$first".toInt
      case first +: List(_*) :+ last => s"$first$last".toInt
      case _ => 0
    }
  }


}

object Part1 {

  def main(args: Array[String]): Unit = {

    val fSource = Source.fromFile("input.txt")
    var result = 0

    for (line <- fSource.getLines()) {
      result += Common.getFirstAndOrLast(line)
    }
    fSource.close()
    println(result)
    }

  }

object Part2 {
  def main(args: Array[String]): Unit = {
    val fSource = Source.fromFile("input.txt")
    val numberMap = Map(
      "one" -> "1", "two" -> "2", "three" -> "3",
      "four" -> "4", "five" -> "5", "six" -> "6",
      "seven" -> "7", "eight" -> "8", "nine" -> "9"
    )

    // extended patterns to match only "first" occurence or number in any form
    val patternFix = (numberMap.keys.mkString("|") + "|" + numberMap.values.mkString("|")).r
    val patternReverse = (numberMap.keys.map(_.reverse).mkString("|") + "|" + numberMap.values.mkString("|")).r

    var result2 = 0

    for (line <- fSource.getLines()) {
      // find first match from the left in normal string
      val firstChange = patternFix.findFirstIn(line) match {
        case Some(x) if x.length() > 1 => patternFix.replaceFirstIn(line, numberMap(x))
        case _ => line}
      // find first match from the left in reversed string (last match)
      val secondChange = patternReverse.findFirstIn(firstChange.reverse) match {
        case Some(x) if x.length() > 1 => patternReverse.replaceFirstIn(firstChange.reverse, numberMap(x.reverse)).reverse
        case _ => firstChange }

      result2 += Common.getFirstAndOrLast(secondChange)
    }
    println(result2)
    fSource.close()
  }
}