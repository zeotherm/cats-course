package part3datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("started something"), 45)
  // Writer[LOG TYPE, VALUE TYPE]
  // 2 - manipulate them with pure FP
  val anIncreaseWriter = aWriter.map(_ + 1) // value increases, logs stays the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found something interesting") // applied to the logs, values stay the same
  val aWriterWithBoth = aWriter.bimap(_ :+ "Found something else neat", _ + 1) // both value and logs changed
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something again", value + 1)
  }
  // flatMap
  import cats.instances.vector._  // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // a Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the values

  // 3 - dump either value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1: rewrite a function that "prints" something with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if( n <= 0) Writer(Vector("Starting"), n)
    else {
      countAndLog(n-1).mapWritten(_ :+ s"$n")
//      countAndLog(n-1).flatMap(_ => Writer(Vector(s"$n"), n))
    }
  }

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n-1)
      println(s"Computed sum(${n-1}) = $lowerSum")
      lowerSum + n
    }
  }

  def writerSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
//    else {
//      writerSum(n-1).mapBoth {
//        (logs, v) =>
//          (logs :+ s"Now at $n", v + n)
//      }
//    }
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- writerSum(n-1)
      _ <- Writer(Vector(s"Computed sum(${n-1}) = $lowerSum"), n)  // logs are combined via semigroup of Vector[String]
    } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on separate threads

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    // ex 1
    countAndSay(10)
    countAndLog(10).written.foreach(println)
    //ex 2
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(writerSum(100))
    val sumFuture2 = Future(writerSum(100))
    val logs1 = sumFuture1.map(_.written) // logs from thread 1
    val logs2 = sumFuture2.map(_.written) // logs from thread 2
    //writerSum(10).written.foreach(println)

  }
}
