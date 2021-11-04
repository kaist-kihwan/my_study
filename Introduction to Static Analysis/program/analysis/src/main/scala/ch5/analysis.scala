package StaticAnalysis

import scala.util.parsing.combinator._

package object ch5 extends Chapter5 {

    object Product_Dom extends Abstract_Domain {
        trait Value
        case class Val(n:Int) extends Value
        case object Infinity extends Value

        type Interval = (Value, Value)
        type Modular = (Int, Int) // remainder, mod
        override type AbstractElement = (Interval, Modular)

        def findModularAbs_union(left:Modular, right:Modular):Modular = {
            val (lr, lm) = left
            val (rr, rm) = right
            val newm = gcd(gcd(lm, rm), lr-rr)
            if (newm == 0) { (lr, newm) }
            else { (lr % newm, newm) }
        }

        def findModularAbs_joint(left:Modular, right:Modular):Option[Modular] = {
            val (lr, lm) = left
            val (rr, rm) = right
            if (lm == 0) {
                if (lr % rm == rr) { Some(left) }
                else { None }
            } else if (rm == 0) {
                if (rr % lm == lr) { Some(right) }
                else { None }
            } else {
                val newm = gcd(lm, rm)
                val k = lr%newm
                if (k == rr%newm) { Some((k, newm)) }
                else { None }
            }
        }

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
            val ((la, lb), lm) = left
            val ((ra, rb), rm) = right
            val new_a = (la, ra) match {
                case (Val(lav), Val(rav)) => {
                    if (lav <= rav) {Val(lav)}
                    else {Val(rav)}
                }
                case (Infinity, _) => Infinity
                case (_, Infinity) => Infinity
            }
            val new_b = (lb, rb) match {
                case (Val(lbv), Val(rbv)) => {
                    if (lbv <= rbv) {Val(rbv)}
                    else {Val(lbv)}
                }
                case (Infinity, _) => Infinity
                case (_, Infinity) => Infinity
            }
            val new_m = findModularAbs_union(lm, rm)
            ((new_a, new_b), new_m)
        }

        def jointElement(left:AbstractElement, right:AbstractElement):Option[AbstractElement] = {
            val ((la, lb), lm) = left
            val ((ra, rb), rm) = right
            val new_a = (la, ra) match {
                    case (Val(lav), Val(rav)) => {
                        if (lav <= rav) {Val(rav)}
                        else {Val(lav)}
                    }
                    case (Infinity, Val(n)) => Val(n)
                    case (Val(n), Infinity) => Val(n)
                }
            val new_b = (lb, rb) match {
                case (Val(lbv), Val(rbv)) => {
                    if (lbv <= rbv) {Val(lbv)}
                    else {Val(rbv)}
                }
                case (Infinity, Val(n)) => Val(n)
                case (Val(n), Infinity) => Val(n)
            }
            val new_m = findModularAbs_joint(lm, rm)
            new_m match {
                case Some(v) => Some( ((new_a, new_b), v) )
                case None => None
            }
        }

        def constraintToElement(option:Bool):AbstractElement = option match {
            case LessThan(_, Scalar(n)) => ((Infinity, Val(n)),(n,0))
            case GreaterThan(_, Scalar(n)) => ((Val(n), Infinity),(n,0))
            case _ => error
        }

        def ele_inclusion(left:AbstractElement, right:AbstractElement):Boolean = {
            val ( ((la,lb),(lr,lm)) , ((ra,rb),(rr,rm)) ) = ( left, right )
            val check1:Boolean = (lb,rb) match {
                case (Val(lbv), Val(rbv)) => (lbv <= rbv)
                case (_, Infinity) => true
                case (Infinity, Val(_)) => false
            }
            val check2:Boolean = (la,ra) match {
                case (Val(lav), Val(rav)) => (lav >= rav)
                case (_, Infinity) => true
                case (Infinity, Val(_)) => false
            }
            val check3:Boolean = {
                if (rm == 0) {lm == 0 && lr == rr}
                else {lm%rm == 0 && lr%rm == rr}
            }
            check1 && check2 && check3
        }

        def evaluate(expr:Expression, abs:Abstraction):Option[AbstractElement] = abs match {
            case Some(mem) => expr match {
                case Scalar(n) => Some( ((Val(n),Val(n)),(0,n)) )
                case Variable(x) => mem.get(x)
                case Plus(e1, e2) => (evaluate(e1, abs), evaluate(e2, abs)) match {
                    case (Some(e1v), Some(e2v)) => {
                        val ((e1a, e1b),(e1r, e1m)) = e1v
                        val ((e2a, e2b),(e2r, e2m)) = e2v
                        val new_a = (e1a, e2a) match {
                            case (Val(e1av), Val(e2av)) => Val(e1av + e2av)
                            case _ => Infinity
                        }
                        val new_b = (e1b, e2b) match {
                            case (Val(e1bv), Val(e2bv)) => Val(e1bv + e2bv)
                            case _ => Infinity
                        }
                        val d = gcd(e1m, e2m)
                        val new_m = ((e1r + e2r)%d, d)
                        Some( ((new_a, new_b), new_m) )
                    }
                    case (_, None) | (None, _) => None
                }
                case Minus(e1, e2) => (evaluate(e1, abs), evaluate(e2, abs)) match {
                    case (Some(e1v), Some(e2v)) => {
                        val ((e1a, e1b),(e1r, e1m)) = e1v
                        val ((e2a, e2b),(e2r, e2m)) = e2v
                        val new_a = (e1a, e2b) match {
                            case (Val(e1av), Val(e2bv)) => Val(e1av - e2bv)
                            case _ => Infinity
                        }
                        val new_b = (e1b, e2b) match {
                            case (Val(e1bv), Val(e2av)) => Val(e1bv - e2av)
                            case _ => Infinity
                        }
                        val d = gcd(e1m, e2m)
                        val new_m = ((e1r - e2r)%d, d)
                        Some( ((new_a, new_b), new_m) )
                    }
                    case (_, None) | (None, _) => None
                }
            }
            case None => None
        }

        def topElement:AbstractElement = ((Infinity, Infinity),(0, 1))

/*
        class Loop_unrolling extends Widening {

        }

        class Delay_widening extends Widening {

        }

        class Widening_threshold extends Widening {

        }
*/
        def widening(init:Abstraction, func:(Abstraction => Abstraction)):Abstraction = {
            var c:Abstraction = None
            var r:Abstraction = None
            r = c
            c = union(c, func(c))
            while (abs_inclusion(c, r)){
                r = c
                c = union(c, func(c))
            }
            r
        }
    }
/*
    class Cardinal_Power(val part_size:Int) extends Abstract_Domain {

        trait Value
        case class Val(n:Int) extends Value
        case object Infinity extends Value

        type Interval = (Value, Value)

        // This array always has same fixed-length (= part_size)
        override type AbstractElement = Array[Interval]

        object Parse extends RegexParsers {
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

        def elementToString(abse:AbstractElement):String = ""

        def unionElement(left:AbstractionElement, right:AbstractionElement):AbstractionElement = {

        }

        def evaluate(expr:Expression, abs:Abstraction):Option[AbstractElement] = {

        }

        def topElement:AbstractionElement
    }

*/

    def run(
        abs_dom:Abstract_Domain, str:String, cond:String
    ):Unit = {
        type Abstraction = abs_dom.Abstraction
        val program: Command = Program(str)
        val abs: Abstraction = abs_dom.apply(cond)
        val result:Abstraction = program match {
            case Skip => abs
            case Sequence(c1, c2) => analyze(c2, analyze(c1, abs, abs_dom), abs_dom)
            case Assign(name, expr) => abs_dom.evaluate(expr, abs) match {
                case Some(v) => abs_dom.update(abs, name, v)
                case None => None
            }
            case Input(name) => abs_dom.update(abs, name, Some(abs_dom.topElement))
            case IfElse(c, t, e) => abs_dom.union(
                analyze(t, abs_dom.filtering(c, abs), abs_dom, widen),
                analyze(e, abs_dom.filtering(negateBool(c), abs), abs_dom, widen)
            )
            case While(c, s) => {
                abs_dom.filtering(negateBool(c), widen.run(
                    abs, (a:Abstraction) => {analyze(s, abs_dom.filtering(c, a), abs_dom, abs_dom.widening)}
                ))
            }
        }
        result match {
            case Some(abst) => println("{%s}".format(abs_dom.toString(abst.keys.toList, abst).mkString(", ")))
            case None => println("bottom")
        }
    }
}
/*
    def run_sparse(str:String, cond):Unit = {

    }

    def run_forward_backward(str:String, cond):Unit = {

    }

}
*/
