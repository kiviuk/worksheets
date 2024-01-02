import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._

type CalcState[Int] = State[List[Int], Int]

def push(symInt: Int): CalcState[Int] =
  State[List[Int], Int] {
    stack => (stack :+ symInt , symInt)
  }
end push

def calc(op: Int => Int => Int): CalcState[Int] =
  State[List[Int], Int] {
    stack => {
      val operands = stack.takeRight(2)
      val res = op(operands(0))(operands(1))
      (stack.dropRight(2) :+ res, res)
    }
  }
end calc 

def evalOne(symbol: String): CalcState[Int] =
  if (symbol.strip().startsWith("+")) {
    calc(a => b => a + b)
  } else if(symbol.strip().startsWith("*")) {
    calc(a => b => a * b)
  } else {
    push(symbol.toInt)
  }
end evalOne  

def evalAll(input: List[String]): CalcState[Int] = {
  val init = State.empty[List[Int], Int]
  input.foldLeft(init)((stack, symbol) => stack.flatMap(_ => evalOne(symbol)))
}
end evalAll  

val program: CalcState[Int] =
  for
    _ <- evalOne("1")
    _ <- evalOne("8")
    _ <- evalOne("4")
    _ <- evalOne("2")
    r <- evalOne("+")
  yield r
end Program

program.runA(Nil).value
program.run(Nil).value

val bigProgram =
  for 
    ans <- evalAll(List("1", "2", "+"))
    ans <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")    
  yield ans
end bigProgram

bigProgram.run(Nil).value

evalAll("1 2 + 3 4 + *".split(" ").toList).run(Nil).value

