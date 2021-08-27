package StaticAnalysis

package object ch5 extends Chapter5 {

    object Product_Dom extends Abstract_Domain {
        object Parse extends Parser {
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

        def apply(str:String):Abstraction = Parse(str)

        def elementToString(abse:AbstractElement):String =

        def update(abs:Abstraction, name:String, value:AbstractElement):Abstraction = {

        }

        def union(left:Abstraction, right:Abstraction):Abstraction = {

        }

        def joint(left:Abstraction, right:Abstraction):Abstraction = {

        }

        def filtering(bool:Bool, abs:Abstraction):Abstraction = {

        }

        def evaluate(expr:Expression, abs:Abstraction):AbstractElement = {

        }

        def top:Abstraction =

    }

    object Cardinal_Power extends Abstract_Domain {
        object Parse extends Parser {
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

        def apply(str:String):Abstraction = Parse(str)

        def elementToString(abse:AbstractElement):String =

        def update(abs:Abstraction, name:String, value:AbstractElement):Abstraction = {

        }

        def union(left:Abstraction, right:Abstraction):Abstraction = {

        }

        def joint(left:Abstraction, right:Abstraction):Abstraction = {

        }

        def filtering(bool:Bool, abs:Abstraction):Abstraction = {

        }

        def evaluate(expr:Expression, abs:Abstraction):AbstractElement = {

        }

        def top:Abstraction
    }

    class Loop_unrolling extends Widening {

    }

    class Delay_widening extends Widening {

    }

    class Widening_threshold extends Widening {

    }

    def run(
        abs_dom:Abstract_Domain, widening:Widening, str:String, cond:String
    ):Unit = analyze(Program(str), abs_dom.apply(cond), abs_dom, widening) match {
        case Some(abst) => println("{%s}".format(abs_dom.toString(abst.keys.toList, abst).mkString(", ")))
        case None => println("bottom")
    }

    def analyze(
        program:Command, abs:Abstraction, abs_dom:Abstract_Domain, widen:Widening
    ):Abstraction = program match {
        case Skip => abs
        case Sequence(c1, c2) => analyze(c2, analyze(c1, abs, abs_dom), abs_dom)
        case Assign(name, expr) => abs_dom.update(abs, name, abs_dom.evaluate(expr, abs))
        case Input(name) => abs_dom.update(abs, name, abs_dom.top)
        case IfElse(c, t, e) => abs_dom.union(
            analyze(t, abs_dom.filtering(c, abs), abs_dom, widen),
            analyze(e, abs_dom.filtering(negateBool(c), abs), abs_dom, widen)
        )
        case While(c, s) => {
            abs_dom.filtering(negateBool(c), widen.run(
                abs, (a:Abstraction) => {analyze(s, abs_dom.filtering(c, a), abs_dom, widen)}
            ))
        }
    }


    def run_sparse(str, cond):Unit = {

    }

    def run_forward_backward(str, cond):Unit = {

    }



}
