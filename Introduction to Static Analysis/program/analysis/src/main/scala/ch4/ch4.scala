package StaticAnalysis

import scala.util.parsing.combinator._

trait Chapter4 {
    trait Command
    trait Expression
    trait Bool
    case class Scalar(n:Int) extends Expression
    case class Variable(name: String) extends Expression
    case class Plus(e1:Expression, e2:Expression) extends Expression
    case class Minus(e1:Expression, e2:Expression) extends Expression
    case class LessThan(left:Expression, right:Expression) extends Bool
    case class GreaterThan(left:Expression, right:Expression) extends Bool
    case object True extends Bool
    case object False extends Bool
    case object Skip extends Command
    case class Sequence(c1:Command, c2:Command) extends Command
    case class Assign(name:String, expr:Expression) extends Command
    case class Input(name:String) extends Command
    case class IfElse(cond:Bool, thenC:Command, elseC:Command) extends Command
    case class While(cond:Bool, statement:Command) extends Command
    case class Goto(label:Expression) extends Command

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
            wrap(expr ~ "<" ~ expr)         ^^ { case l ~ _ ~ r => LessThan(l, r) }    |
            "true"                          ^^ { case _ => True }                         |
            "false"                         ^^ { case _ => False }
        lazy val command: Parser[Command] =
            "skip"                                  ^^ { case _ => Skip }                         |
            wrap(command ~ ";" ~ command)           ^^ { case c1 ~ _ ~ c2 => Sequence(c1, c2)}    |
            wrap(str ~ ":=" ~ expr)                 ^^ { case x ~ _ ~ e => Assign(x, e) }         |
            wrap("Input" ~> str)                    ^^ { case x => Input(x) }                     |
            wrap("If" ~> bool ~ command ~ command)  ^^ { case b ~ tc ~ ec => IfElse(b, tc, ec) }  |
            wrap("While" ~> bool ~ command)         ^^ { case b ~ c => While(b, c) }              |
            wrap("Goto" ~> expr)                    ^^ { case e => Goto(e) }
        def apply(str: String): Command = parseAll(command, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
    }

    type LabelMap = Map[Int, Int]

    trait Val
    case class Value(n:Int) extends Val
    case object Infinity extends Val
    trait AbstractionElement
    case class Interval(left:Val, right:Val) extends AbstractionElement
    type Abstract = Map[String, AbstractionElement]
    type Abstraction = Option[Abstract]

    object Condition extends RegexParsers {
        def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
        lazy val num: Parser[Val] =
            """-?\d+""".r   ^^ (x => Value(x.toInt))  |
            "inf".r         ^^ ( _ => Infinity)
        lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
        lazy val element: Parser[(String,AbstractionElement)] =
            str ~ "=" ~ "[" ~ num ~ "," ~ num ~ "]"    ^^ { case x ~ _ ~ _ ~ a ~ _ ~ b ~ _ => (x -> Interval(a, b)) }
        lazy val expr: Parser[Abstraction] =
            wrap(repsep(element, ","))              ^^ { case ms => Some(Map() ++ ms) }     |
            "bottom".r                              ^^ { case _ => None }
        def apply(str:String): Abstraction = parseAll(expr, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
        // def testNum(s:String):Val = parseAll(num, s).get
        // def testStr(s:String):String = parseAll(str, s).get
    }
    def run(str: String, cond:String): Unit
}
