package StaticAnalysis

import scala.util.parsing.combinator._

trait ProgramExcerpt {
    trait Command
    trait Expression
    trait Bool
    case class Scalar(n:Int) extends Expression
    case class Variable(name: String) extends Expression
    case class Plus(e1:Expression, e2:Expression) extends Expression
    case class Minus(e1:Expression, e2:Expression) extends Expression
    case class LessThan(name:String, cons:Scalar) extends Bool
    case class GreaterThan(name:String, cons:Scalar) extends Bool
    case class AndGate(left:Bool, right:Bool) extends Bool
    case class OrGate(left:Bool, right:Bool) extends Bool
    case object True extends Bool
    case object False extends Bool
    case object Random extends Bool
    case object Skip extends Command
    case class Sequence(c1:Command, c2:Command) extends Command
    case class Assign(name:String, expr:Expression) extends Command
    case class Input(name:String) extends Command
    case class IfElse(cond:Bool, thenC:Command, elseC:Command) extends Command
    case class While(cond:Bool, statement:Command) extends Command
}

trait Chapter5 extends Mainframe with ProgramExcerpt {

    object Program extends RegexParsers {
        def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
        lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
        lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
        lazy val expr: Parser[Expression] =
            int                             ^^ { case n => Scalar(n) }          |
            wrap("+" ~> expr ~ expr)        ^^ { case l ~ r => Plus(l,r) }      |
            wrap("-" ~> expr ~ expr)        ^^ { case l ~ r => Minus(l,r) }     |
            str                             ^^ { case x => Variable(x) }
        lazy val bool: Parser[Bool] =
            wrap("<" ~> str ~ int)          ^^ { case x ~ n => LessThan(x, Scalar(n))}    |
            wrap(">" ~> str ~ int)          ^^ { case x ~ n => GreaterThan(x, Scalar(n))}
        lazy val command: Parser[Command] =
            wrap("skip")                            ^^ { case _ => Skip }                         |
            wrap(command ~ ";" ~ command)           ^^ { case c1 ~ _ ~ c2 => Sequence(c1, c2)}    |
            wrap(str ~ ":=" ~ expr)                 ^^ { case x ~ _ ~ e => Assign(x, e) }         |
            wrap("Input" ~> str)                    ^^ { case x => Input(x) }                     |
            wrap("If" ~> bool ~ command ~ command)  ^^ { case b ~ tc ~ ec => IfElse(b, tc, ec) }  |
            wrap("While" ~> bool ~ command)         ^^ { case b ~ c => While(b, c) }
        def apply(str: String): Command = parseAll(command, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
    }
}
