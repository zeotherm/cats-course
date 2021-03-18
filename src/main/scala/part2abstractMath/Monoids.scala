package part2abstractMath

object Monoids {

  // what problem do monoids solve that semigroups do not?
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._  // import the |+| extension method
  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
//  def combineFold[T](ls: List[T])(implicit semigroup: Semigroup[T]): T =
//    ls.foldLeft(/* WHAT?! */)(_ |+| _)

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1024
  val zero = intMonoid.empty // will return the zero value for the type, which is 0 for ints

  import cats.instances.string._         // bring the implicit Monoid[String] in scope
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6)) // Some(9)

  // extension methods for Monoids - |+|
  //import cats.syntax.monoid._ // either this or cats.syntax.semigroup._, but only one at a time to have access to |+|
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T](ls: List[T])(implicit monoid: Monoid[T]): T = ls.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Map[String, Int]
  // hintL don't construct a monoid your self, use an import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._
  val massivePhoneBook = combineFold(phonebooks)

  // TODO 3 - shopping cart and online stores with Monoids
  // hint: define your own monoid - Monoid.instance
  // hint 2: use combineByFold
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty[String], 0.0),
    (lhs, rhs) => ShoppingCart(lhs.items ++ rhs.items, lhs.total + rhs.total)
  )

  def checkOut(scs: List[ShoppingCart]): ShoppingCart = combineFold(scs)

  val sc1 = ShoppingCart(List("Adapter", "Plug"), 34.99)
  val sc2 = ShoppingCart(List("Laptop"), 999.99)

  val totalShoppingCart = checkOut(List(sc1, sc2))

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "monoids")))
    println(massivePhoneBook)
    println(totalShoppingCart)
  }
}
