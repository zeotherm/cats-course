package part3datamanipulation

import cats.Semigroup

import scala.util.Try

object DataValidation {

  import cats.data.Validated
  // acts like an either -- left is undesirable, right is what you want
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non negative
    - n <= 100
    - n must be even
   */
  val primesUnder100: Set[Int] = Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
    47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

  def testNumber(n: Int): Either[List[String], Int] = {
//    def test1(n: Int): Either[String, Int] = {
//      if (primesUnder100.contains(n)) {
//        Right(n)
//      } else {
//        Left("n is not prime")
//      }
//    }
//
//    def test2(n: Int): Either[String, Int] = {
//      if (n >= 0) {
//        Right(n)
//      } else {
//        Left("n must be non-negative")
//      }
//    }
//
//    def test3(n: Int): Either[String, Int] = {
//      if ( n <= 100) {
//        Right(n)
//      } else {
//        Left("n must be 100 or less")
//      }
//    }
//
//    def test4(n: Int): Either[String, Int] = {
//      if (n % 2 == 0) {
//        Right(n)
//      } else {
//        Left("n must be even")
//      }
//    }
//    val results: Seq[Either[String, Int]] = List(test1(n), test2(n), test3(n), test4(n))
//    def collapseResults(rs: Seq[Either[String, Int]], ans: Either[List[String], Int]) = {
//      if (rs.isEmpty) ans
//      else {
//        rs.head match {
//          case Left(value) => if ans.left {(rs.tail, Left(value :: ans.left))
//        }
//      }
//    }
    val isNotEven: List[String] = if( n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Must be non-negative")
    val isTooBig: List[String] = if (n <= 100) List() else List("Number must be less than 100")
    val isNotPrime: List[String] = if (primesUnder100.contains(n)) List() else List("Number must be prime")

    if( n % 2 == 0 && n >= 0 && n <= 100 && primesUnder100.contains(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }
  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(primesUnder100.contains(n), n, List("Number must be prime")))

  // chain
  val q = aValidValue.andThen(_ => anInvalidValue)
  // test a valid value with ensure
  aValidValue.ensure(List("Something went wrong"))(_%2 == 0)
  // transform
  aValidValue.map(_ + 1)        // maps the valid type
  aValidValue.leftMap(_.length) // maps the error type
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]
    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must not be blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains('@'), email, List("email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password must be at least 10 characters long"))

    /*
      fields will be
      - name
      - e-mail
      - password

      rules are
      - name, email, and password MUST be specified
      - name must not be blank
      - email must contain "@"
      - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      import cats.instances.string._

// my Solution
//      implicit val combineStringViaLeft: Semigroup[String] = Semigroup.instance[String]((l, _) => l)
//
//      val x = Validated.cond(form.keySet.equals(Set("name", "email", "password")), "Success", List("All elements (name, email, password) must be present"))
//      if (x.isValid) {
//        x.combine(Validated.cond(!form("name").isEmpty, "Success", List("Name cannot be blank")))
//         .combine(Validated.cond(form("email").contains('@'), "Success", List("Must enter a valid e-mail address")))
//         .combine(Validated.cond(form("password").length >= 10, "Success", List("Password must be more than 10 characters")))
//      } else x
      getValue(form, "name").andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "User Registration Complete")

    }
  }

  import cats.syntax.validated._
  val meaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something ent wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(q)
    val formInput = Map("name" -> "Matt Ford", "email" -> "matt@email.com", "password" -> "pw12345678")
    println(FormValidation.validateForm(formInput))

    val formInputBad1 = Map("email" -> "matt@email.com", "password" -> "pw12345678")
    println(FormValidation.validateForm(formInputBad1))

    val formInputBad2 = Map("name" -> "", "email" -> "matt@email.com", "password" -> "pw12345678")
    println(FormValidation.validateForm(formInputBad2))

    val formInputBad3 = Map("name" -> "Matt Ford", "email" -> "matt email.com", "password" -> "pw12345678")
    println(FormValidation.validateForm(formInputBad3))

    val formInputBad4 = Map("name" -> "Matt Ford", "email" -> "matt@email.com", "password" -> "pw1234")
    println(FormValidation.validateForm(formInputBad4))

    val formInputMultiBad = Map("name" -> "Matt Ford", "email" -> "matt email.com", "password" -> "pw1234")
    println(FormValidation.validateForm(formInputMultiBad))
  }
}
