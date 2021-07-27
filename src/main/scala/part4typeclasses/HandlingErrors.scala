package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure from Applicative
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(pred: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // must be typed with the same type that the Either has as the undesired value
  val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("Something wrong")  // type it in case it is chained with other ops
                                                                     // Either[String, Int] = Left("Something wrong")
  // "recover"
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "badness" => 44
    case _ => 89
  } // second part is a partial function that will determine how to deal with a given error and return the appropriate type
  // "recoverWith"
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "badness" => monadErrorEither.pure(44) // wrapper type, ErrorOr[Int]
    case _ => Left("Something else") // ErrorOr[Int]
  }
  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100) // turn the success into a failure if the predicate is not met

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception)  //store this as information rather then letting the JVM throw it
                                                                                  // Try[Int] => Failure(exception)
  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._  // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]

  // ApplicativeError : Applicative :: MonadError : Error
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleErrorWith, handleError

  val extendedSuccess = 42.pure[ErrorsOr] // reuires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError : ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // ensure
  val testedSuccess = success.ensure("something bad")(_ > 100)


  def main(args: Array[String]): Unit = {

  }
}
