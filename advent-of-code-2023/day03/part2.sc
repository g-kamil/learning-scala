import scala.util.{Using, Try}
import scala.io.Source
import scala.math.{min, max}
import scala.util.matching.Regex


val isValidNumber = (number: String) => Try(number.toInt).isSuccess

def collectLeft(idx: Int, arr: String)(row: Array[String]): String = {
  if (idx < 0 || !isValidNumber(row(idx))) arr
  else collectLeft(idx - 1, row(idx) + arr)(row)
}

def collectRight(idx: Int, arr: String)(row: Array[String]): String = {
  if (idx >= row.length || !isValidNumber(row(idx))) arr
  else collectRight(idx + 1, arr + row(idx))(row)
}


Using(scala.io.Source.fromFile("input.txt")) { source =>

  val characterMatrix: Array[Array[String]] = source.getLines().map(_.toArray.map(_.toString)).toArray

  val maxY = characterMatrix.length - 1
  val maxX = characterMatrix(0).length - 1
  val numPattern = "(\\d+)".r
  var result = 0

  for ( y <- 0 to maxY; x <- 0 to maxX) {

    val currentSign = characterMatrix(y)(x)
    var surrounding = ""

    if (currentSign == "*") {

      // build surrounding as single string
      for (r <- y - 1 to y + 1;
           c <- x - 1 to x + 1
           if ((0 <= r && r <= maxY) && (0 <= c && c <= maxX)) // skip out of index
      ) {

        surrounding += {
          if c == (x - 1) then collectLeft(c, "")(characterMatrix(r))
          else if c == (x + 1) then collectRight(c, "")(characterMatrix(r)) + "." // dot as "new line separator"
          else characterMatrix(r)(c)
        }
      }
      // find all numbers in surrounding string and parse them to Int
      val foundNumbers = numPattern.findAllIn(surrounding).toList.map(_.toInt)

      // if exactly two numbers found,
      if (foundNumbers.length == 2) result += foundNumbers.reduceLeft(_ * _)
    }
  }

  println(result)
}