package StaticAnalysis

import scala.util.parsing.combinator._

package object ch5 extends Chapter5 {

    class Product_Domain(str: String) extends Abstract_Domain{

        trait Value
        case class Val(n:Int) extends Value
        case object Infinity extends Value

        type Interval = (Value, Value)
        type Modular = (Int, Int) // remainder, mod
        override type AbstractElement = (Interval, Modular)
        
        object Parse extends RegexParsers {
            def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
            lazy val num: Parser[Value] =
                """-?\d+""".r   ^^ (x => Val(x.toInt))  |
                "inf".r         ^^ ( _ => Infinity)
            lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
            lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
            lazy val element: Parser[(String,AbstractElement)] =
                str ~ "=" ~ "[" ~ num ~ "," ~ num ~ "]" ~ "*" ~ "(" ~ int ~ "," ~ int ~ ")"   ^^ {
                    case x ~ _ ~ _ ~ a ~ _ ~ b ~ _ ~ _ ~ _ ~ r ~ _ ~ m ~ _ => (x, ((a, b), (r, m)))
                }
            lazy val expr: Parser[Abstraction] =
                wrap(repsep(element, ","))              ^^ { case ms => Some(Map() ++ ms) }     |
                "bottom".r                              ^^ { case _ => None }
            def apply(str:String): Abstraction = parseAll(expr, str) match {
                case Success(result, _) => result
                case failure : NoSuccess => scala.sys.error(failure.msg)
            }
        }

        var contents: Abstraction = Parse(str)

        
    }

}