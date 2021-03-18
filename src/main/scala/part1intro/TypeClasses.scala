package part1intro

object TypeClasses {
  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  // part 2 - create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJSON(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJSON(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJSON(value: Person): String =
      s"""
         |{ "name" : ${value.name}, "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJSON[T](ls: List[T])(implicit  serializer: JSONSerializer[T]): String =
    ls.map(v => serializer.toJSON(v)).mkString("[", ",", "]")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJSON(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJSON(List(Person("Alice", 23), Person("Xavier", 45))))
    val bob = Person("Bob", 35)
    import JSONSyntax._
    println(bob.toJson)
  }
}
