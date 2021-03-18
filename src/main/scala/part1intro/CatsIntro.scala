package part1intro

object CatsIntro {

  // Eq
  // val aComparison = 2 == "a string" -- wrong, will trigger a compiler warning, will always return false

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._

  // part 3 - use the type class API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // returns a boolean (false)
  // val anUnsafeComparison = intEquality.eqv(2, "a string") -- doesn't compile

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // boolean (false)
  val neqComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "a string" -- doesn't compile either
  // extension methods are only visible in the presence of the right type class instance

  // part 5 - extending the type class operations to composite types, e.g. Lists
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3)

  // part 6 - create a type class instance for a custom type that is not automatically handled by cats
  case class ToyCar(model: String, price: Double) // define equality on price alone
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (lhs, rhs) =>
    lhs.price == rhs.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // true

}
