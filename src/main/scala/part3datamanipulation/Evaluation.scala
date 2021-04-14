package part3datamanipulation

object Evaluation {
  /*
    Cats makes the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and everytime you request it
    - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    456
  }

  val redoEval = Eval.always {
    println("Computing again!")
    78832
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later!")
    4912
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical to above

  // TODO 1: predict the output
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // 1st: Now, later, again, again
  // 2nd: again, again

  val dontRecompute = redoEval.memoize // hold the internal value without needing to recompute expression

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap" }
    .map { step1 => println("Step 2"); s"$step1 the put your left hand on the neck" }
    .memoize // remember the value up to this point
    .map { steps12 => println("Step 3, more complicated"); s"$steps12 then with the right hand strike the strings" }

  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  //def defer[T](eval: => Eval[T]): Eval[T] = Eval.later {eval.value} // Eval.later(()).flatMap(_ => eval)
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)
  // TODO 3: rewrite the method with evals instead of regular values
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.later { list }
    else defer(reverseEval(list.tail).flatMap(r => Eval.later(r :+ list.head)))  // else reverseEval(list.tail).map(_ :+ list.head)
    //else reverseEval(list.tail).map(_ :+ list.head)
    // Question: Why use map versus flatmap here?
  }

  def main(args: Array[String]): Unit = {
    println("In main")
//    println(instantEval.value)
//    println(redoEval.value)
//    println(redoEval.value)
//    println(delayedEval.value)
//    println(delayedEval.value)
//    println(composedEvaluation.value)
//    println(composedEvaluation.value)
//    println(evalEx1.value)
//    println(evalEx1.value)
//    println(dontRecompute.value)
//    println(dontRecompute.value)
//
//    println(tutorial.value)
//    println(tutorial.value)
//    val deffered = defer(Eval.now {
//      println("Now!")
//      42
//    })
//    println(deffered.value)
    println(reverseList(List(1, 2, 3, 5, 7, 9, 11, 13)))
    println(reverseEval((1 to 10000).toList).value)

  }
}
