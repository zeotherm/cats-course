package part1intro

object Implicits {

  // implicit classe - one argument wrappers over values
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  // always take a single argument
  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

//  val impersonableString = new ImpersonableString("Peter")
//  impersonableString.greet

  val greeting = "Peter".greet  // Behind the scenes the compiler does a new ImpersonableString("Peter").greet -- it
                                // is basically an extension method

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(2) // implicit argument 10 is passe by the compiler--seems like basic currying

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}}
         |""".stripMargin
  }
  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         | "{${value.productElementName(0)}" : "${value.productElement(0)}}"
         |""".stripMargin.trim
  }
  // all case classes extend Product
  case class Cat(catName: String)
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))
  // in the background the compiler will do:
  // val catsToJson = listToJson(List((Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)

  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("Matt")))
  }
}
