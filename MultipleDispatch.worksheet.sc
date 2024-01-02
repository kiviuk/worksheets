// "The expression problem is a new name for an old problem. The goal is to define a datatype by cases,
// where one can add new cases to the datatype and new functions over the datatype ...
// while retaining static type safety


// The MultiMethod allows flexibility in adding new types to a pre-existing 
// Data Model and adding new functions (behaviour) to existing behavior 
// but it is not statically type safe !!!
class MultiMethod[A, R](default: => R) {
  private var handlers: List[PartialFunction[A, R]] = Nil

  def apply(args: A): R = {
    handlers find {
      _.isDefinedAt(args)
    } map {
      _.apply(args)
    } getOrElse default
  }

  def size = handlers.length

  def defImpl(handler: PartialFunction[A, R]) = {
    handlers +:= handler
  }
}

val multiMethod = new MultiMethod[Int, String](default = "Default")

val myCases: PartialFunction[Int, String] = {
  case x if x > 0 => s"Positive number: $x"
  case x if x < 0 => s"Negative number: $x"
  case x if x == 0 => s"Zero number: $x"
}

myCases(1)

multiMethod.defImpl {
  case x if x > 10 => s"Positive number greater 10: $x"
  case x if x < -10 => s"Negative number smaller -10: $x"
}

multiMethod.defImpl {myCases}

multiMethod.size

println (multiMethod(1))
println (multiMethod(1000))


// multiMethod.defImpl { case x if x > 0 => s"Positive number: $x" }
// multiMethod.defImpl { case x if x == 0 => "Zero" }
// multiMethod.defImpl { case x if x < 0 => s"Negative number: $x" }

// println(multiMethod(5)) // Prints: Positive number: 5
// println(multiMethod(-3)) // Prints: Negative number: -3
// println(multiMethod(0)) // Prints: Zero
// println(multiMethod(7)) // Prints: Default