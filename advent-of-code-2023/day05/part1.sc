import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val fileContent = (Using(scala.io.Source.fromFile("input.txt")) { source =>
   source.getLines().mkString("\n")
}).getOrElse("")

val fileBatches = fileContent.split("\n\n")

val startingSeeds = fileBatches.head.split(":")(1).trim.split(" ").map(BigInt(_)).toArray

var currentSeeds = ArrayBuffer[BigInt]()
val proceededSeeds = ArrayBuffer[BigInt]()
val toSkip = ArrayBuffer[BigInt]()

currentSeeds ++= startingSeeds

for (batch <- fileBatches.tail) {

  val splittedBatch = batch.split("\n")


  for (batchLine <- splittedBatch.tail) {

    // unpack values from line
    val Array(destination, source, offset) = batchLine.split(" ").map(BigInt(_))

    // checking if seeds match mapping
    currentSeeds.foreach { x =>
      if (x >= source && x < source + offset && !toSkip.contains(x)) {
        proceededSeeds += destination + (x - source)
        toSkip += x
      }
    }

  }

  currentSeeds = proceededSeeds ++ currentSeeds.filterNot(toSkip.contains)

  toSkip.clear()
  proceededSeeds.clear()

}

println(s"min = ${currentSeeds.min}")