package part2abstractMath

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]  // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x+1))
  // applicable to Option, Try, Future, ... etc

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T] // T (right) is the desirable side, string would then be the error message
  type ErrorOr[T] = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(x => if (x % 2 == 0) Right(x+1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if( orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO 1: the service layer API of a web application
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localHost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String) : M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      resp <- service.issueRequest(conn, payload)
    } yield resp
  // DO NOT CHANGE THE ABOVE CODE

  /*
      Reqs:
      - if the host and port are found in the configuration map, then return a M containing a connection with those
        values otherwise, fail, according to the logic of the type M
      - the issueRequest method will return a M containing the string "request (payload) has been accepted", if the
        payload is less then 20 characters, otherwise it should fail according to the logic of the type M.
   */

  val badConfig = Map(
    "host" -> "localHost",
    "prat" -> "4040"
  )
  val goodPayload = "Test message"
  val badPayload = "This payload message is way to long"

  // Use Option for M:
  import cats.instances.option._
  val optionMonad = Monad[Option]
  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      if (cfg.contains("host") && cfg.contains("port")) {
        Some(Connection(cfg("host"), cfg("port")))
      } else {
        Option.empty[Connection]
      }

      // could also utilize the existing mechanics around Option:
      // for {
      //   h <- cfg.get("host")
      //   p <- cfg.get("port")
      // } yield Connection(h, p)
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length < 20) {
        Some(s"request $payload has been accepted")
      } else {
        Option.empty[String]
      }
    }
  }

  val connectionRequestOptionFor: Option[String] = for {
    conn <- OptionHttpService.getConnection(config)
    resp <- OptionHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestOptionFor1: Option[String] = for {
    conn <- OptionHttpService.getConnection(badConfig)
    resp <- OptionHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestOptionFor2: Option[String] = for {
    conn <- OptionHttpService.getConnection(config)
    resp <- OptionHttpService.issueRequest(conn, badPayload)
  } yield resp

  val badConnectionRequestOptionFor3: Option[String] = for {
    conn <- OptionHttpService.getConnection(badConfig)
    resp <- OptionHttpService.issueRequest(conn, badPayload)
  } yield resp

  // Use Try for M:
  import cats.instances.try_._
  val tryMonad = Monad[Try]
  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = {
      if (cfg.contains("host") && cfg.contains("port")) {
        Success(Connection(cfg("host"), cfg("port")))
      } else {
        Failure[Connection](new java.lang.Exception("bad config"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] = {
      if (payload.length < 20) {
        Success(s"request $payload has been accepted")
      } else {
        Failure[String](new java.lang.Exception("bad payload"))
      }
    }
  }

  val connectionRequestTryFor: Try[String] = for {
    conn <- TryHttpService.getConnection(config)
    resp <- TryHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestTryFor1: Try[String] = for {
    conn <- TryHttpService.getConnection(badConfig)
    resp <- TryHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestTryFor2: Try[String] = for {
    conn <- TryHttpService.getConnection(config)
    resp <- TryHttpService.issueRequest(conn, badPayload)
  } yield resp

  val badConnectionRequestTryFor3: Try[String] = for {
    conn <- TryHttpService.getConnection(badConfig)
    resp <- TryHttpService.issueRequest(conn, badPayload)
  } yield resp

  // Use Either for M:
  import cats.instances.either._
  type MyLoadingOr[T] = Either[String, T]
  val eitherMonad = Monad[MyLoadingOr]
  object EitherHttpService extends HttpService[MyLoadingOr] {
    override def getConnection(cfg: Map[String, String]): MyLoadingOr[Connection] = {
      if (cfg.contains("host") && cfg.contains("port")) {
        Right(Connection(cfg("host"), cfg("port")))
      } else {
        Left("bad config - either")
      }
    }

    override def issueRequest(connection: Connection, payload: String): MyLoadingOr[String] = {
      if (payload.length < 20) {
        Right(s"request $payload has been accepted")
      } else {
        Left("bad payload - either")
      }
    }
  }

  val connectionRequestEitherFor: MyLoadingOr[String] = for {
    conn <- EitherHttpService.getConnection(config)
    resp <- EitherHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestEitherFor1: MyLoadingOr[String] = for {
    conn <- EitherHttpService.getConnection(badConfig)
    resp <- EitherHttpService.issueRequest(conn, goodPayload)
  } yield resp

  val badConnectionRequestEitherFor2: MyLoadingOr[String] = for {
    conn <- EitherHttpService.getConnection(config)
    resp <- EitherHttpService.issueRequest(conn, badPayload)
  } yield resp

  val badConnectionRequestEitherFor3: MyLoadingOr[String] = for {
    conn <- EitherHttpService.getConnection(badConfig)
    resp <- EitherHttpService.issueRequest(conn, badPayload)
  } yield resp

  def main(args: Array[String]): Unit = {

    println(getResponse(OptionHttpService, "Hello Option"))
    println(getResponse(EitherHttpService, "Hello Either!"))

    println(connectionRequestOptionFor)
    println(badConnectionRequestOptionFor1)
    println(badConnectionRequestOptionFor2)
    println(badConnectionRequestOptionFor3)

    println(connectionRequestTryFor match {
      case Success(s) => s
      case Failure(e) => s"Failed. Reason: $e"
    })
    println(badConnectionRequestTryFor1 match {
      case Success(s) => s
      case Failure(e) => s"Failed. Reason: $e"
    })
    println(badConnectionRequestTryFor2 match {
      case Success(s) => s
      case Failure(e) => s"Failed. Reason: $e"
    })
    println(badConnectionRequestTryFor3 match {
      case Success(s) => s
      case Failure(e) => s"Failed. Reason: $e"
    })

    println(connectionRequestEitherFor)
    println(badConnectionRequestEitherFor1)
    println(badConnectionRequestEitherFor2)
    println(badConnectionRequestEitherFor3)
  }
}
