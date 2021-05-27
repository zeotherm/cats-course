package part4typeclasses

object Applicatives {

  // Applicatives = Functors + pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List[2]
  val aSweetOption = 2.pure[Option] // Some(2)


  // Monads extend Applicatives
  // Applicatives extend Functors

  // Validated doesn't respect the monad laws, but it can be used as an applicative
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43)  // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map
  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  // def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T] = ???  // This is already implemented in the typeclass
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }
  // Applicative can also implement product from semigroupal in the presence of the ap method
  // Applicatives extends Semigroupals

  def main(args: Array[String]): Unit = {

  }
}
