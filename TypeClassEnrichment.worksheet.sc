// Third-party type class
trait ThirdPartyTypeClass[A] {
  def someMethod(value: A): String
}

// Your extension trait
trait MyExtension[A] {
  def myAdditionalMethod(value: A): String
}

given AString: ThirdPartyTypeClass[String] with {
  def someMethod(value: String): String = value.toUpperCase()
}

// Enrichment using Scala 3 extension method
extension [A](value: A)(using ev: ThirdPartyTypeClass[A])
  def myAdditionalMethod: String = s"My additional method for ${ev.someMethod(value)}"

// Usage
val instance: String = "Hello, Scala!"

// You can now use myAdditionalMethod on instances of String
val result: String = instance.myAdditionalMethod // String = My additional method for HELLO, SCALA!