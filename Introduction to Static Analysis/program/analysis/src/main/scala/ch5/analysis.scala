package StaticAnalysis

package object ch5 extends Chapter5 {

    object Product_Dom extends Abstract_Domain {
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
        }
    }

    object Cardinal_Power extends Abstract_Domain {
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
        }
    }

    class Loop_unrolling extends Widening {

    }

    class Delay_widening extends Widening {

    }

    class Widening_threshold extends Widening {

    }

    def run(abs_dom, widening, str, cond):Unit = {

    }

    def run_sparse(str, cond):Unit = {

    }

    def run_forward_backward(str, cond):Unit = {

    }



}
