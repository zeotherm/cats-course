package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value // run returns an Eval
  // state is an abstraction for iterative computations

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // Pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${5*s}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  val f = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val g = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${5*s}")
  val compositeResult = f.andThen {
    case (newState, firstResult) => (firstResult, g(newState))
  }

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = {
    State(sc => (ShoppingCart(item :: sc.items, sc.total + price), sc.total + price))
  }

  val mattsCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("SMB Classic Cartridge", 1300)
    _ <- addToCart("OG NES", 299)
    total <- addToCart("CRT TV", 39)
  } yield total

  // TODO 2: pure mental gymnastics
  // returns a State data structure that when run, will not change the state but will issue the value f(A)
  def inspect[A, B](f: A => B): State[A, B] = State { s => (s, f(s)) }
  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State { s => (s, s) }
  // returns a State data structure that, when run, return Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State { _ => (value, ())}
  // returns a State data structure that, when run, will return Unit and sets the stats to f(state)
  def modify[A](f: A => A): State[A, Unit] = State { s => (f(s), ()) }

  // methods available
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int] (_ * 2)
  } yield (a, b, c)



  def main(args: Array[String]): Unit = {
    println(compositeTransformation2.run(10).value)
    println(compositeResult(10))

    println(mattsCart.run(ShoppingCart(List(), 0)).value)
  }
}
