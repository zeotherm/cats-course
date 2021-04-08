package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT // option transformer
  import cats.instances.list._ // fetch an implicit OptionT[List] <<< Monad[List]
  import cats.instances.future._

  // option transformer
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // Either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45)) into a EitherT
    //EitherT(Future[Either[String, Int]](Right(45)))

  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]  // Future[Either[String, T]] String is undesired type, T is good resp
  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Unreachable Server: $server")) //EitherT(Future[Either[String, Int]](Left(s"Unreachable Server: $server")))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1 implement:
  // hint: call getBandWidth twice and combine the results

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = { // True if server's bandwidth summed up is > 250
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield (b1 + b2) > 250
  }
  // TODO 2 implement:
  // hint: call canWithStandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    def transformFunc(resp: Either[String, Boolean]): Either[String, String] = {
      resp match {
        case Right(s) if s => Right(s"Servers $s1 and $s2 CAN handle the surge")
        case Right(s) if !s => Left(s"The servers $s1 and $s2 cannot handle the total bandwidth")
        case Left(reason) => Left(s"The servers $s1 and $s2 cannot handle the total bandwidth: $reason")
      }
    }
    canWithstandSurge(s1, s2).transform(transformFunc)
  }


  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)

    val resultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture.foreach(println)

    val resultFuture2 = generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture2.foreach(println)

    val resultFuture3 = generateTrafficSpikeReport("server2.rockthejvm.com", "cnn.com").value
    resultFuture3.foreach(println)
  }
}
