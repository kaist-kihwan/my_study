package StaticAnalysis

package object ch5 extends Chapter5 {

    object Product_Dom extends Abstract_Domain {
        trait Value
        case class Val(n:Int) extends Value
        case object Infinity extends Value

        type Interval = (Value, Value)
        type Modular = (Int, Int) // remainder, mod
        override type AbstractElement = (Interval, Modular)

        object Parse extends Parser {
            def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
            lazy val num: Parser[Val] =
                """-?\d+""".r   ^^ (x => Val(x.toInt))  |
                "inf".r         ^^ ( _ => Infinity)
            lazy val int: Parser[Int] = """-?\d+""".r
            lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
            lazy val element: Parser[(String,AbstractionElement)] =
                str ~ "=" ~ "[" ~ num ~ "," ~ num ~ "]" ~ "*" ~ "(" ~ int ~ "," ~ int ~ ")"   ^^ {
                    case x ~ _ ~ _ ~ a ~ _ ~ b ~ _ ~ _ ~ _ ~ n1 ~ _ ~ n2 ~ _ => (x -> ((a, b), (r, m)))
                }
            lazy val expr: Parser[Abstraction] =
                wrap(repsep(element, ","))              ^^ { case ms => Some(Map() ++ ms) }     |
                "bottom".r                              ^^ { case _ => None }
            def apply(str:String): Abstraction = parseAll(expr, str) match {
                case Success(result, _) => result
                case failure : NoSuccess => scala.sys.error(failure.msg)
            }
        }

        def apply(str:String):Abstraction = Parse(str)

        def elementToString(abse:AbstractElement):String = {
            val (i, (n1, n2)) = abse
            "{%s * (%d, %d)}".format(intervalToString(i), n1, n2)
        }
        def intervalToString(i:Interval):String = i match {
            case (Val(n1), Val(n2)) => "[%d, %d]".format(n1, n2)
            case (Val(n1), Infinity) => "[%d, inf]".format(n1)
            case (Infinity, Val(n2)) => "[-inf, %d]".format(n2)
            case (Infinity, Infinity) => "[-inf, inf]"
        }

        def unionElement(left:AbstractElement, right:AbstractElement):AbstractElement = {
            val ((la, lb),(lr, lm)) = left
            val ((ra, rb),(rr, rm)) = right
            val new_i = (
                (la, ra) match {
                    case (Val(lav), Val(rav)) => {
                        if (lav <= rav) {lav}
                        else {rav}
                    }
                    case (Infinity, _) => Infinity
                    case (_, Infinity) => Infinity
                },
                (lb, rb) match {
                    case (Val(lbv), Val(rbv)) => {
                        if (lbv <= rbv) {rbv}
                        else {lbv}
                    }
                    case (Infinity, _) => Infinity
                    case (_, Infinity) => Infinity
                }
            )
            val new_m =
            (new_i, new_m)
        }

        def jointElement(left:AbstractElement, right:AbstractElement):Option[AbstractElement] = {
            
        }

        def filterElement(b:Bool, abs:Abstraction):Option[Memory] = b match {
            case LessThan(name, cons) =>
            case GreaterThan(name, cons) =>
            case _ => error
        }

        def evaluate(expr:Expression, abs:Abstraction):AbstractElement = {

        }

        def topElement:AbstractionElement = ((Infinity, Infinity),(0, 1))

    }

    object Cardinal_Power extends Abstract_Domain {

        trait Value
        case class Val(n:Int) extends Value
        case object Infinity extends Value

        type Interval = (Value, Value)

        class StatePartition (var greater0: Interval, var equal0: Interval, var less0: Interval)

        override type AbstractElement = StatePartition

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

        def unionElement(left:AbstractionElement, right:AbstractionElement):AbstractionElement = {

        }

        def filtering(bool:Bool, abs:Abstraction):Abstraction = {

        }

        def evaluate(expr:Expression, abs:Abstraction):Option[AbstractElement] = {

        }

        def topElement:AbstractionElement
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
        case Input(name) => abs_dom.update(abs, name, Some(abs_dom.topElement))
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
