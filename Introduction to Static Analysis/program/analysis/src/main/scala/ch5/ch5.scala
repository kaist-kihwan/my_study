package StaticAnalysis

import scala.util.parsing.combinator._

// program excerpt trait.
// it represents syntax of target programming language.
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
    case object Random extends Bool // non-deterministic boolean. depends on runtime.
    case object Skip extends Command
    case class Sequence(c1:Command, c2:Command) extends Command
    case class Assign(name:String, expr:Expression) extends Command
    case class Input(name:String) extends Command
    case class IfElse(cond:Bool, thenC:Command, elseC:Command) extends Command
    case class While(cond:Bool, statement:Command) extends Command
    case class Assert(name:String, denominator:Scalar, remainder:Scalar) extends Command
}

// parser of input program
trait Chapter5 extends Mainframe with ProgramExcerpt {

    object Program extends RegexParsers {
        def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
        lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
        lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
        lazy val expr: Parser[Expression] =
            int                             ^^ { case n => Scalar(n) }              |
            wrap(expr ~ "+" ~ expr)         ^^ { case l ~ _ ~ r => Plus(l,r) }      |
            wrap(expr ~ "-" ~ expr)         ^^ { case l ~ _ ~ r => Minus(l,r) }     |
            str                             ^^ { case x => Variable(x) }
        lazy val bool: Parser[Bool] =
            wrap(str ~ "<=" ~ int)              ^^ { case x ~ _ ~ n => LessThan(x, Scalar(n)) }     |
            wrap(str ~ ">=" ~ int)              ^^ { case x ~ _ ~ n => GreaterThan(x, Scalar(n)) }  |
            wrap(bool ~ "&&" ~ bool)            ^^ { case l ~ _ ~ r => AndGate(l, r) }              |
            wrap(bool ~ "||" ~ bool)            ^^ { case l ~ _ ~ r => OrGate(l, r) }               |
            wrap("true")                        ^^ { case _ => True }                               |
            wrap("false")                       ^^ { case _ => False }                              |
            wrap("random")                      ^^ { case _ => Random }
        lazy val command: Parser[Command] =
            wrap("skip")                                    ^^ { case _ => Skip }                         |
            wrap(command ~ ";" ~ command)                   ^^ { case c1 ~ _ ~ c2 => Sequence(c1, c2)}    |
            wrap(str ~ ":=" ~ expr)                         ^^ { case x ~ _ ~ e => Assign(x, e) }         |
            wrap("Input" ~> str)                            ^^ { case x => Input(x) }                     |
            wrap("If" ~> bool ~ command ~ command)          ^^ { case b ~ tc ~ ec => IfElse(b, tc, ec) }  |
            wrap("While" ~> bool ~ command)                 ^^ { case b ~ c => While(b, c) }
        def apply(str: String): Command = parseAll(command, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
    }

    // return the negation of input boolean formula
    def negateBool(b:Bool):Bool = b match {
        case LessThan(name, cons) => GreaterThan(name, cons)
        case GreaterThan(name, cons) => LessThan(name, cons)
        case AndGate(left, right) => OrGate(negateBool(left), negateBool(right))
        case OrGate(left, right) => AndGate(negateBool(left), negateBool(right))
        case True => False
        case False => True
        case Random => Random
    }
}
