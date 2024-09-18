import scala.util.Using
import scala.io.Source
import scala.math.pow
import scala.collection.mutable.Map

Using(scala.io.Source.fromFile("input.txt")) { source =>

  val lines = source.getLines().map(_.split(":")(1))
  var currentGame = 1
  val scratchCards = Map.empty[Int, Int]

  for (line <- lines) {
    // add current card to mapping or increase it by 1 if already exists
    scratchCards(currentGame) = scratchCards.getOrElse(currentGame, 0) + 1

    val Array(winning, choosen) = line.replaceAll(" +", " ").split("\\|").map(_.trim).map(_.split(" "))

    val matching = winning.intersect(choosen).length

    for (i <- currentGame + 1 to currentGame + matching if matching > 0) {
      scratchCards(i) = scratchCards.getOrElse(i, 0) + 1 * scratchCards(currentGame)
    }

    currentGame += 1
  }
  println(scratchCards.values.sum)
}