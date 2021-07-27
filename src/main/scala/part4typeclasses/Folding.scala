package part4typeclasses

import cats.{Eval, Monoid}

object Folding {
  // TODO - implement all in terms of foldLeft
  object ListExercises {
    def map[A, B](ls: List[A])(f: A => B): List[B] = {
      ls.foldLeft(List.empty[B])((acc, elem) => acc :+ f(elem))
    }
    def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
      ls.foldLeft(List.empty[B])((acc, elem) => acc ++ f(elem))   // ls.foldLeft(List.empty[B])((acc, elem) => acc.foldRight(f(elem))(_ :: _)
    }
    def filter[A](ls: List[A])(pred: A => Boolean): List[A] = {
      ls.foldLeft(List.empty[A])((acc, elem) => if (pred(elem)) acc :+ elem else acc)
    }
    def combineAll[A](ls: List[A])(implicit monoid: Monoid[A]): A = {
      ls.foldLeft(monoid.empty)((acc, elem) => monoid.combine(acc, elem))  // ls.foldLeft(monoid.empty)(monoid.combine)
    }
  }

  import cats.Foldable
  import cats.instances.list._  // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)  // 6
  import cats.instances.option._ // implicit Foldable[Option]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight is stack-safe regardless of the implementation of the container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }
  import cats.instances.int._
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3))
  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // map elements and combine their results => "123"

  // nesting
  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1, 2, 3).combineAll // req implicit presence of Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    import ListExercises._
    val initList = List(1, 2, 3, 4)
    val strList = List("one", "two", "three", "four")
    println(ListExercises.map(initList)(_ + 2))
    println(ListExercises.filter(initList)(_ % 2 == 0))
    println(ListExercises.flatMap(initList)(a => List(a, a + 10)))
    println(ListExercises.combineAll(strList))

    val numbers = (1 to 10).toList
    println(flatMap(numbers)(x => (1 to x).toList))
  }
}
