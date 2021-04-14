package part3datamanipulation

object Readers {
  /*
    - config file => initial data structures
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long) : String = "dispatched" // select * from the db table and return the status of the orderID
    def getLastOrderId(username: String): Long = 3432 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration("matt", "mattsPassword", "localhost", 1234, 8, "matt@email.com")
  // cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(config => DbConnection(config.dbUsername, config.dbPassword))
  val dbCon = dbReader.run(config)

  // Reader takes an input type I, and output type O -- Reader[I, O]
  val mattsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val mattsOrderStatus: String = mattsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map and flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */


  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From $emailReplyTo to $address >>> $contents"
  }
  // TODO 1: email a user

  val emailReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))
  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"
    val emailToSend = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      toSend <- emailReader.map(_.sendEmail(userEmail, s"Your last order has the status: $orderStatus"))
    } yield toSend

//    val emailAlt = for {
//      lastOrderId <- dbReader.map(_.getLastOrderId(username))
//      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
//      emailService <- emailReader
//    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")
//    emailAlt.run(config)

    emailToSend.run(config)
  }

  // TODO 2: what programming pattern do Readers remind you of?
  // Dependency Injection

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("matt"))
    println(emailUser("risha", "risha@email.com"))
  }
}
