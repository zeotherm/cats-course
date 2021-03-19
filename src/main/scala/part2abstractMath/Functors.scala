package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1,2,3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor
  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumber = listFunctor.map(List(1,2,3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option._ // include Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._ // include Functor[Try]
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // Generalizing an API
  def do10xList(ls: List[Int]): List[Int] = ls.map(_ * 10)
  def do10xOption(o: Option[Int]): Option[Int] = o.map(_ * 10)
  def do10Try(t: Try[Int]): Try[Int] = t.map(_ * 10)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
    }
  }

  val baseTree = Tree.branch(3, Tree.branch(2, Tree.leaf(1), Tree.leaf(5)), Tree.leaf(6))

  // extension method - map
  import cats.syntax.functor._
  val tree2: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree2.map(_ + 1)

  // TODO 2: Write a shorter do10xShorter method using extension methods
  def do10xShorter[F[_] : Functor](cs: F[Int]): F[Int] = cs.map(_ * 10)


  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
    println(do10x(Option(2)))
    println(do10x(Try(42)))
    println(do10x(baseTree))
    println(incrementedTree)

    println(do10xShorter(List(1,2,3)))
    println(do10xShorter(Option(2)))
    println(do10xShorter(Try(42)))



  }
}
