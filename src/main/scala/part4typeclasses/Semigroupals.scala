package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemiGroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ //Implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  import cats.instances.list._ // Monad[List]
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODO: implement productWithMonads
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    // monad.flatMap(fa)(a => monad.map(fb)(b => (a, b))), or my solution below
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }
  // MONADS EXTEND SEMIGROUPALS

  // example: Validated - use case for combining instances of validated with out needing to follow the monad laws
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit SemiGroup[List[_]]
                                                   // implements product of two validated types by independently
                                                   // combining the error type and the value type according to
                                                   // their semigroup

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("something wrong", "something else wrong")),
    Validated.invalid(List("this can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( //implemented in terms of map/flatMap
    Left(List("something wrong", "something else wrong")),
    Left(List("this can't be right"))
  ) // lose the final result because the flatMap method will short circuit once the first error is found

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g)) -- true for Either, but NOT true for Validated

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa.zip(fb)
  }

  val mySemigroupalZip = zipListSemigroupal.product(List(1, 2), List("a", "b"))

  def main(args: Array[String]): Unit = {
    // println(aTupledList)
    println(invalidsCombination)
    println(eitherCombination)
    println(mySemigroupalZip)
  }
}
