import scala.collection.mutable.ListBuffer

sealed trait Animal
trait Dog extends Animal
trait Chicken extends Animal

trait ClinicTrait[-A <: Animal] {
  def accept(animal: A): Int
}

case class Clinic[-A <: Animal]() extends ClinicTrait[A] {
  private val animals = ListBuffer.empty[A]
  override def accept(animal: A): Int = 
    animals += animal
    this.animals.length
}

val animalClinic: Clinic[Animal] = new Clinic
animalClinic.accept(new Dog {})
animalClinic.accept(new Chicken {})
val dogClinic: Clinic[Dog] = animalClinic
// the below line would not compile..
//dogClinic.accept(new Chicken {})


// type Animal = Dog | Chicken

// enum Dog:
//   case Labrador(name: String)
//   case Poodle(name: String)

// enum Chicken:
//   case RegularChicken
//   case FancyChicken

// // Usage
// val myPet: Animal = Dog.Labrador("Buddy")

// import Dog._ 
// import Chicken._ 

// myPet match
//   case dog: Dog => 
//     dog match
//       case Labrador(name) => println(s"A Labrador named $name")
//       case Poodle(name) => println(s"A Poodle named $name")
//   case chicken: Chicken => 
//     chicken match
//       case RegularChicken => println("A regular chicken")
//       case FancyChicken => println("A fancy chicken")