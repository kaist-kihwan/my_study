package StaticAnalysis

package object ch3 extends Chapter3 {
  trait Val
  case class Value(n:Int) extends Val
  case class Infinity extends Val
  trait AbstractionElement
  case class Interval(left:Val, right;Val) extends AbstractionElement

  type Abstraction = Map[String, AbstractionElement]

  def run(str:String):Unit = {
    val expr = Program(str)
    val v = analysis(expr)
    println("{%s}".format(abstractionToString(v, v.keys).mkString(", ")))
  }

  def abstractionToString(a:Abstraction, keys:List[String]):List[String] = keys match {
    case head::tail => "%s->%s".format(head, a.get(head).get) :: abstractionToString(a, tail)
    case Nil => List[String]()
  }

  def valueUnion(i1:AbstractionElement, i2:AbstractionElement):AbstractionElement = (i1, i2) match {
    case (Interval(a1,b1), Interval(a2,b2)) => Interval(
      (a1,a2) match {
        case (Value(n1), Value(n2)) => Value(n1.min(n2))
        case _ => Infinity
      }, (b1,b2) match {
        case (Value(n1), Value(n2)) => Value(n1.max(n2))
        case _ => Infinity
      }
    )
  }

  def _abstractionUnion(a1:Abstraction, a2:Abstraction, keys:List[String]):Abstraction = keys match {
    case head :: tail => a1.get(head) match {
      case Some(v) => v match {
        case Interval(a, b) =>
      }
      case None =>
    } + _abstractionUnion(a1, a2, tail)
    case Nil => Map[String, AbstractionElement]()
  }

  def abstractionUnion(a1:Abstraction, a2:Abstraction):Abstraction = {
    var keys = (a1.keySet union a2.keySet).toList
    _abstractionUnion(keys)
  }

  def abstractVal(expr:Expression, precond:Abstraction):AbstractionElement = expr match {
    case Scalar(n) => Interval(n,n)
    case Variable(x) => precond.get(x) match {
      case Some(v) => v
      case None => throw Error("Unbound variable")
    }
    case Plus(l, r) =>
      val al = abstractVal(l, precond)
      val ar = abstractVal(r, precond)
      al match {
        case Interval(l1, l2) => ar match {
          case Interval(r1, r2) => Interval(
             (l1, r1) match {
               case (Value(n1), Value(n2)) => Value(n1+n2)
               case _ => Infinity
             }, (l2, r2) match {
               case (Value(n1), Value(n2)) => Value(n1+n2)
               case _ => Infinity
             })
        }
      }
    case Minus(l, r) =>
      val al = abstractVal(l, precond)
      val ar = abstractVal(r, precond)
      al match {
        case Interval(l1, l2) => ar match {
          case Interval(r1, r2) => Interval(
             (l1, r2) match {
               case (Value(n1), Value(n2)) => Value(n1-n2)
               case _ => Infinity
             }, (l2, r1) match {
               case (Value(n1), Value(n2)) => Value(n1-n2)
               case _ => Infinity
             })
        }
      }
  }

  def abstractVal(expr:Bool, precond:Abstraction):Abstraction = expr match {
    case LessThan(x,n) => precond.get(x) match {
      Some(v) => v match {
        case Interval(a,b) => {
          if(n>b){precond}
          else if(n>a){precond + (x->Interval(a,n))}
          else {Map[String, AbstractionElement]()}
        }
      }
      None => Map[String, AbstractionElement]()
    }
    case GreaterThan(x,n) => precond.get(x) match {
      Some(v) => v match {
        case Interval(a,b) => if(n<=a){precond}
          else if(n<b){precond + (x->Interval(n,b))}
          else {Map[String, AbstractionElement]()}
      }
      None => Map[String, AbstractionElement]()
    }
    case Equal(x,n) => precond.get(x) match {
      Some(v) => v match {
        case Interval(a,b) => if(a<=n && n<=b){precond + (x->Interval(n,n))}
          else {Map[String, AbstractionElement]()}
      }
      None => Map[String, AbstractionElement]()
    }
  }

  def analysis(expr:Command, precond:Abstraction):Abstraction = expr match {
    case Skip => precond
    case Sequence(c1, c2) => analysis(c2, analysis(c1,precond))
    case Assign(x, e) => precond + (x->abstractVal(e, precond))
    case Input(name) => precond + (name->Interval(Infinity,Infinity))
    case IfElse(c,t,e) =>
    case While(c, s) =>
  }
}
