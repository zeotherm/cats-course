package part4typeclasses

import cats.{Applicative, Apply, FlatMap}

object WeakerMonads {
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B]
    // hint: Apply extends Functor (have map function)
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(wf)(f => f(a)))
    //         |  |        /    \     \/
    //         |  |     M[A=>B] A=>B  B
    //         |  |     \_____  _____/
    //       M[A] A =>       M[B]
    }
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f : A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.syntax.flatMap._  // flatMap extension method
  import cats.syntax.functor._  // map extension method

  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)


  def main(args: Array[String]): Unit = {

  }
}
