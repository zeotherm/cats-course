package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // specific API
  def reduceInt(ls: List[Int]): Int = ls.reduce(naturalIntSemigroup.combine)
  def reduceString(ls: List[String]): String = ls.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](ls: List[T])(implicit semigroup: Semigroup[T]): T = ls.reduce(semigroup.combine)

  // TODO 1: support a new type for a semigroup
  // hint use the same pattern we used with Eq previously
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (lhs, rhs) =>
    Expense(Math.max(lhs.id, rhs.id), lhs.amount + rhs.amount)
  }

  // extension methods from Semigroup - |+| (read: combine)
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // TODO 2: implement reduceThings2 with the |+| operator
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)

  def reduceThings3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)



  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInt(numbers))
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceString(strings))

    // general API
    println(reduceThings(numbers))  // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings))  // compiler injects the implicit Semigroup[String]
    import cats.instances.option._  // compiler will produce an implicit Semigroup[Option[Int]]
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements
    // same for any type with an implicit Semigroup
    val numberOptions: List[Option[Int]] = Option.empty[Int] :: numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers
    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    val expenses = List(Expense(1, 0.50), Expense(2, 0.75), Expense(4, 1.0))
    println(reduceThings(expenses))

    println(reduceThings2(expenses))
  }
}
