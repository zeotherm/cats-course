package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1: How do you create all the combinations of (number, char)?
  val combinationsList: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  val combinationsOption: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n,c)))
  val combinationsOptionFor: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('Z')
  // TODO 1.3: how do you create the combination of (number, char)?
  val combinationsFuture: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationsFutureFor: Future[(Int, Char)] = for {
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
    // TODO: implement this
    def map[A, B](ma: M[A])(f : A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(3) // List(3)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad: Monad[Future] = Monad[Future] // will require an implicit ExecutionContext
  val aFuture: Future[Int] = futureMonad.pure(17) // Future(17)
  val aTransformedFuture: Future[String] = futureMonad.flatMap(aFuture)(x => if (x < 18) Future("kid") else Future("adult"))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // generalized API
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a,b)))

  // extension methods - weirder imports - pure, flatMap (bind)
  import cats.syntax.applicative._ // contains pure
  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] - Some(1)
  val oneList: List[Int] = 1.pure[List]     // implicit Monad[List] - List(1)

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

//  def >>=[M[_], A, B](f: A => M[B])(implicit monad: Monad[M]): M[B] =
//    monad.flatMap()(f)

  // TODO 3: implement the map method in MyMonad
  // Monads extend Functors
  val oneMonadOption: Option[Int] = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped2: Option[Int] = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor: Option[Int] = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    for {
      a <- ma
      b <- mb
    } yield (a, b) // same as ma.flatMap(a => mb.map(b => (a, b)))
  }


  def main(args: Array[String]): Unit = {
//    println(combinationsListFor)
//    println(combinationsOptionFor)
//    println(combinationsFutureFor)
//    println(aTransformedFuture)
//
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
    println(getPairsFor(numberOption, charOption))
    println(getPairsFor(numbersList, charsList))
  }
}
