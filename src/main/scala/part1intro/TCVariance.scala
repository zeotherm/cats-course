package part1intro

object TCVariance {
  import cats.Eq
  import cats.instances.int._     // bring in Eq[Int] TC instance
  import cats.instances.option._  // con construct an Eq[Option[Int]] TC Instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None  // Eq[Some[Int]] is not found --> Eq[Some[]] is not a subtype of Eq[Option]

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]  // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // variance affect how TC Instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow") // implementation not important
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC Instance for Animal is also applicable to Cats
  // rule 1: contravariant type classes can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses INVARIANT TCs
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - the compiler will use CatsShow as the implicit
    // println(organizeShow[Animal])
  }



}
