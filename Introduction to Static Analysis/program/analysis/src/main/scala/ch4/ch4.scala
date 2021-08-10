package StaticAnalysis

import scala.util.parsing.combinator._

trait Chapter4 extends Mainframe {
    trait Command
    trait Expression
    trait Bool
    case class Scalar(n:Int) extends Expression
    case class Variable(name: String) extends Expression
    case class Plus(e1:Expression, e2:Expression) extends Expression
    case class Minus(e1:Expression, e2:Expression) extends Expression
    case class LessThan(x:String, n:Expression) extends Bool
    case class GreaterThan(x:String, n:Expression) extends Bool
    case object True extends Bool
    case object False extends Bool
    case class Skip(label:Int) extends Command
    case class Sequence(label:Int, c1:Command, c2:Command) extends Command
    case class Assign(label:Int, name:String, expr:Expression) extends Command
    case class Input(label:Int, name:String) extends Command
    case class IfElse(label:Int, cond:Bool, thenC:Command, elseC:Command) extends Command
    case class While(label:Int, cond:Bool, statement:Command) extends Command
    case class Goto(label:Int, label:Expression) extends Command

    type LabelNext = Array[(Int,Int)]

    trait CommandLabel
    case object Skip_L extends CommandLabel
    case object Sequence_L extends CommandLabel
    case class Assign_L(name:String, expr: Expression) extends CommandLabel
    case class Input_L(name:String) extends CommandLabel
    case class IfElse_L(cond:Bool) extends CommandLabel
    case class While_L(cond: Bool) extends CommandLabel
    case class Goto_L(lab:Expression) extends CommandLabel
    case object Endpoint_L extends CommandLabel
    type LabelMap = Array[CommandLabel]

    trait Value
    case class Val(n:Int) extends Value
    case object Infinity extends Value
    type Abs_Element = (Value, Value)
    type Abs_Els = Map[String, Abs_Element] // M#
    type Abs_Memory = Option[Abs_Els] // M# (including bottom)
    type Abstraction = Map[Int, Abs_Memory] // L -> M#
    type State = (Int, Abs_Memory) // L, M#

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
            wrap(str ~ "<=" ~ expr)          ^^ { case x ~ _ ~ n => LessThan(x, n) }    |
            wrap(str ~ ">=" ~ expr)          ^^ { case x ~ _ ~ n => GreaterThan(x, n) } |
            "true"                           ^^ { case _ => True }                      |
            "false"                          ^^ { case _ => False }
        lazy val command: Parser[Command] =
            "skip"                                  ^^ { case _ => Skip(-1) }                         |
            wrap(command ~ ";" ~ command)           ^^ { case c1 ~ _ ~ c2 => Sequence(-1, c1, c2)}    |
            wrap(str ~ ":=" ~ expr)                 ^^ { case x ~ _ ~ e => Assign(-1, x, e) }         |
            wrap("Input" ~> str)                    ^^ { case x => Input(-1, x) }                     |
            wrap("If" ~> bool ~ command ~ command)  ^^ { case b ~ tc ~ ec => IfElse(-1, b, tc, ec) }  |
            wrap("While" ~> bool ~ command)         ^^ { case b ~ c => While(-1, b, c) }              |
            wrap("Goto" ~> expr)                    ^^ { case e => Goto(-1, e) }
        def apply(str: String): Command = parseAll(command, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
    }

    object Condition extends RegexParsers {
        def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
        lazy val num: Parser[Value] =
            """-?\d+""".r   ^^ (x => Val(x.toInt))  |
            "inf".r         ^^ ( _ => Infinity)
        lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
        lazy val element: Parser[(String,Abs_Element)] =
            str ~ "=" ~ "[" ~ num ~ "," ~ num ~ "]"    ^^ { case x ~ _ ~ _ ~ a ~ _ ~ b ~ _ => (x -> (a, b)) }
        lazy val expr: Parser[Abs_Memory] =
            wrap(repsep(element, ","))              ^^ { case ms => Some(Map() ++ ms) }     |
            "bottom".r                              ^^ { case _ => None }
        def apply(str:String): Abs_Memory = parseAll(expr, str) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
        // def testNum(s:String):Val = parseAll(num, s).get
        // def testStr(s:String):String = parseAll(str, s).get
    }
    def run(str: String, cond:String): Unit
}
