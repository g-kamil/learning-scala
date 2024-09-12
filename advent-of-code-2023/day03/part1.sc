import scala.util.{Using, Try}
import scala.io.Source
import scala.math.{min, max}
import scala.util.matching.Regex


Using(scala.io.Source.fromFile("input.txt")) { source =>
  
  val characterMatrix: Array[Array[String]] = source.getLines().map(_.toArray.map(_.toString)).toArray
  
  val maxY = characterMatrix.length - 1
  val maxX = characterMatrix(0).length - 1
  var result = 0
  var currentPart = ""
  val removalPattern = "([.0-9])".r

  for ( y <- 0 to maxY; x <- 0 to maxX) {
    val currentSign = characterMatrix(y)(x)

    val isNum = Try(currentSign.toInt).isSuccess

    if (isNum) currentPart += currentSign

    if (!currentPart.isEmpty && (!isNum || x == maxX)) {

      val offset = if (currentSign == ".") 1 else 0
      // collect surrounding string
      val above = characterMatrix(max(0, y - 1)).slice(max(0, x - currentPart.length - offset), x + 1).mkString("")
      val left = characterMatrix(y)(max(0, x - currentPart.length - offset))
      val right = characterMatrix(y)(x)
      val below = characterMatrix(min(maxY, y + 1)).slice(max(0, x - currentPart.length - offset), x + 1).mkString("")

      val surroundingString = left + above + right + below
      val clearedString = removalPattern.replaceAllIn(surroundingString, "")

      if (!clearedString.isEmpty) result += currentPart.toInt

      // for debugging
      // println(s"$currentPart => $surroundingString vs $clearedString ${!clearedString.isEmpty}")

      currentPart = ""
    }
    
  }
  println(result)
}