package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
    we have
      - a List[String]
      - String => Future[Int]
    we want a Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) {(acc, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- acc
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1
  import cats.syntax.applicative._ // for the pure method
  import cats.syntax.flatMap._     // for flatMap
  import cats.syntax.functor._     // for map
  def listTraverseM[F[_] : Monad, A, B](ls: List[A])(func: A => F[B]): F[List[B]] = {
    val emptyList: F[List[B]] = List.empty[B].pure[F]
    ls.foldLeft(emptyList) { (wAccumulator, elem) =>
      val wElem: F[B] = func(elem)
      for {
        acc <- wAccumulator
        e <- wElem
      } yield acc :+ e
    }
  }

  import cats.syntax.apply._       // for mapN
  def listTraverseA[F[_] : Applicative, A, B](ls: List[A])(func: A => F[B]): F[List[B]] = {
    ls.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement = func(element)
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  }
  // TODO 2
  def listSequence[F[_]: Applicative, A](ls: List[F[A]]): F[List[A]] = listTraverseA(ls)(identity)

  // TODO 3
  import cats.instances.vector._
  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4))) // ??? Vector[List[Int]] => Vector(List( 1, 2, 3, 4)) XX -> All the possible 2-tuples (cartesian product)
  val allTriples = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // ??? Vector(List(1, 2, 3, 4, 5, 6)) X -> Collection of all the possible 3-pairs

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] = // equivalent is forAll with an Option return on a list
    listTraverseA[Option, Int, Int](list)(n => Some(n).filter(predicate))

  // TODO 4 - what is the result of
  val allTrue = filterAsOption(List(2, 4, 2))(_ % 2 == 0) // -> Some(List(2, 4, 6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // -> Some(List(2)) ... might be Nones in here, but that wouldn't make a lot of sense == None, since folding and combining Some(x) + None = None

  import cats.data.Validated
  import cats.instances.list._  // Semigroup[List] => Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseA[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // Validated is only an instance of Applicative, no need for a Monad in scope
  // TODO 5 - what's the result of
  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) //
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) //

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    // TODO 6
    // hint
    import cats.Id // fake wrapper for which cats can create the strongest types possible
    //type Identity[T] = T
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence and traverse methods
  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)

    println(allBandwidthsCats)
  }
}
