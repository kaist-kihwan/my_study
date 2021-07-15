package StaticAnalysis

import scala.util.parsing.combinator._

trait Chapter3 {
    trait Program
    trait Command extends Program
    trait Expression
    trait Bool
    case class Scalar(v: Int) extends Expression
    case class Variable(name: String) extends Expression
    case class Plus(e1:Expression, e2:Expression) extends Expression
    case class Minus(e1:Expression, e2:Expression) extends Expression
    case class Mult(e1:Expression, e2:Expression) extends Expression
    case class CompOp(name:String, cons:Scalar) extends Bool
    case object Skip extends Command
    case class Sequence(c1:Command, c2:Command) extends Command
    case class Assign(name:String, expr:Expression) extends Command
    case class Input(name:String) extends Command
    case class IfElse(cond:Bool, thenC:Command, elseC:Command) extends Command
    case class While(cond:Bool, statement:Command) extends Command

    object Program extends RegexParsers {
        def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
        lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
        lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
        lazy val expr: Parser[Program] =
            int                             ^^ { case n => Scalar(n) }          |
            wrap("+" ~> expr ~ expr)        ^^ { case l ~ r => Plus(l,r) }      |
            wrap("-" ~> expr ~ expr)        ^^ { case l ~ r => Minus(l,r) }     |
            wrap("*" ~> expr ~ expr)        ^^ { case l ~ r => Mult(l,r) }      |
            str                             ^^ { case x => Variable(x) }        |

    }
}
