// Generics
// source: https://www.youtube.com/watch?v=ozcY_K-ij20


object Generics extends App {

  /*
    - what generics are
    - how to use them
    - why they exist
   */

  // in python
  // aList = [1,2,3] -> compiler doesn't know it's a list until I run the code

  // static type system <- every type is known to compiler before code run
  val aList2 = List(1,2,3)

  // make assumptions about the data
  val aNumber = 42 // I could define it explicitly as val aNumber: Int = 42; but I used type inferation
  val aMultiplication = aNumber * 10 // this is possible
  // val anotherMultiplication = false * 10 // this not

  val aList: List[Int] = List(1,2,3)
  val secondElement = aList.apply(1)
  val anAppendedList = aList :+ 4 // [1,2,3,4] as a List[Int]

  // reason why generics exist
  // 1. we can make assumptions about the types we work with
  // 2. we can reuse logic on potentially unrelated types

  // e.g. general interface of the list datastructure
  trait MyList {
    def head: Int
    def tail: MyList
  }

  object Empty extends MyList {
    override def head: Int = throw new NoSuchElementException()
    override def tail: MyList = throw new NoSuchElementException()
  }

  case class NonEmpty(h: Int, t: MyList) extends MyList {
    override def head: Int = h
    override def tail: MyList = t
  }

  val someNumbers: MyList = NonEmpty(1, NonEmpty(2, NonEmpty(3, Empty)))
  // if I would like to do above for string as well, I would need to copy the code, so instead I can use generics
  // example below is still no generic and is bad, because Any supports different types, which means that this
  // MyListAny could have at the same time items of different type
  // SO I LOST TYPE SAFETY
  trait MyListAny {
    def head: Any
    def tail: MyListAny
  }

  object EmptyAny extends MyListAny {
    override def head = throw new NoSuchElementException()
    override def tail = throw new NoSuchElementException()
  }

  case class NonEmptyAny(h: Any, t: MyListAny) extends MyListAny {
    override def head = h
    override def tail = t
  }
  val someNumbers2: MyListAny = NonEmptyAny(1, NonEmptyAny('2', NonEmptyAny(3, EmptyAny)))

  // So let's now create GENERICS
  trait GoodList[A] {// A - is type argument, usually named with one letter
    def head: A
    def tail: GoodList[A]
  }

  case class GoodEmpty[A]() extends GoodList[A] { // I couldn't define it with "object" because if I use generics, there would be several implementation for each type
    override def head = throw new NoSuchElementException()
    override def tail = throw new NoSuchElementException()
  }

  case class GoogNoEmpty[A](h: A, t: GoodList[A]) extends GoodList[A] {
    override def head: A = h
    override def tail: GoodList[A] = t
  }

  val goodNumbers: GoodList[Int] = GoogNoEmpty(1, GoogNoEmpty(2, GoogNoEmpty(3, GoodEmpty())))
  val firstNumber = goodNumbers.head
  val lastElements = goodNumbers.tail

  // Generic with multiple type args
  trait MyMap[K, V] {
    def put(key: K, value: V): MyMap[K, V]
    def get(key: K): V
  }

  // Generic methods
  def lastElement[A](list: GoodList[A]): A =
    if (list == GoodEmpty[A]()) throw new NoSuchElementException
    else if (list.tail == GoodEmpty[A]()) list.head
    else lastElement(list.tail)

  def main(args: Array[String]): Unit = {

  }
}
