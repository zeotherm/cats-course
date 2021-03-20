package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1: How do you create all the combinations of (number, char)?
  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  val combinationsOption = numberOption.flatMap(n => charOption.map(c => (n,c)))
  val combinationsOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('Z')
  // TODO 1.3: how do you create the combination of (number, char)?
  val combinationsFuture = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationsFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
      Pattern
      - wrapping a value into a M (monadic) value
      - the flatMap mechanism

      MONADS
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future] // will require an implicit ExecutionContext
  val aFuture = futureMonad.pure(17) // Future(17)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => if (x < 18) Future("kid") else Future("adult"))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // generalized API
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a,b)))

  def main(args: Array[String]): Unit = {
//    println(combinationsListFor)
//    println(combinationsOptionFor)
//    println(combinationsFutureFor)
//    println(aTransformedFuture)
//
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)

  }
}
