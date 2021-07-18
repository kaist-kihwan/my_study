package StaticAnalysis

package object ch3 extends Chapter3 {

  def run(str:String, cond:String):Unit = {
    val expr = Program(str)
    val precond = Condition(cond)
    analysis(expr, precond) match {
        case Some(v) => println("{%s}".format(abstractionToString(v, v.keys.toList).mkString(", ")))
        case None => println("bottom")
    }
  }

  def abstractionToString(a:Abstract, keys:List[String]):List[String] = keys match {
    case head::tail => "%s->%s".format(head, elementToString(a.get(head).get)) :: abstractionToString(a, tail)
    case Nil => List[String]()
  }

  def elementToString(i:AbstractionElement):String = i match {
      case Interval(a,b) => (a,b) match {
          case (Value(n), Value(m)) => "[%d,%d]".format(n, m)
          case (Infinity, Value(n)) => "[-inf,%d]".format(n)
          case (Value(n),Infinity) => "[%d,inf]".format(n)
          case (Infinity,Infinity) => "[-inf,inf]"
      }
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

  def _abstractionUnion(a1:Abstract, a2:Abstract, keys:List[String]):Abstract = keys match {
    case head :: tail => _abstractionUnion(a1, a2, tail).++(a1.get(head) match {
        case Some(v1) => v1 match {
            case Interval(a, b) => a2.get(head) match {
                case Some(v2) => v2 match {
                    case Interval(c,d) => Map[String,AbstractionElement](head -> Interval(
                        (a,c) match {
                            case (Value(av), Value(cv)) => Value(av.min(cv))
                            case _ => Infinity
                        }, (b,d) match {
                            case (Value(bv), Value(dv)) => Value(bv.max(dv))
                            case _ => Infinity
                        }
                    ))
                }
                case None => Map[String,AbstractionElement](head -> Interval(a,b))
            }
        }
        case None => a2.get(head) match {
            case Some(v) => v match {
                case Interval(a,b) => Map[String,AbstractionElement](head -> Interval(a,b))
            }
            case None => Map[String, AbstractionElement]()
        }
    })
    case Nil => Map[String, AbstractionElement]()
  }

  def abstractionUnion(a1:Abstraction, a2:Abstraction):Abstraction = (a1, a2) match {
    case (Some(av1), Some(av2)) => {
        var keys = (av1.keySet union av2.keySet).toList
        Some(_abstractionUnion(av1, av2, keys))
    }
    case (None, Some(av)) => Some(av)
    case (Some(av),None) => Some(av)
    case (None,None) => None
  }

  def abstractVal(expr:Expression, precond:Abstract):AbstractionElement =  expr match {
        case Scalar(n) => Interval(Value(n),Value(n))
        case Variable(x) => precond.get(x) match {
          case Some(v) => v
          case None => throw new Exception("Unbound variable")
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
        case Minus(l, r) => {
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
  }

  def abstractVal(expr:Bool, cond:Abstraction):Abstraction = cond match {
      case Some(precond) => expr match {
        case LessThan(x,Scalar(n)) => precond.get(x) match {
          case Some(v) => v match {
            case Interval(k,m) => (k,m) match {
              case (Value(a),Value(b)) => {
                if(n>b){Some(precond)}
                else if(n>a){Some(precond + (x->Interval(Value(a),Value(n))))}
                else {None}
              }
              case (Infinity, Value(a)) => {
                  if(n<a){Some(precond + (x->Interval(Infinity,Value(n))))}
                  else {Some(precond)}
              }
              case (Value(a), Infinity) => {
                  if(a<=n){Some(precond + (x->Interval(Value(a),Value(n))))}
                  else {None}
              }
              case (Infinity, Infinity) => {
                  Some(precond + (x->Interval(Infinity,Value(n))))
              }
            }
          }
          case None => None
        }
        case GreaterThan(x,Scalar(n)) => precond.get(x) match {
            case Some(Interval(k,m)) => (k,m) match {
              case (Value(a),Value(b)) => {
                if(n<a){Some(precond)}
                else if(n<b){Some(precond + (x->Interval(Value(n),Value(b))))}
                else {None}
              }
              case (Infinity, Value(a)) => {
                  if(n<a){Some(precond + (x->Interval(Value(n),Value(a))))}
                  else {None}
              }
              case (Value(a), Infinity) => {
                  if(a<=n){Some(precond + (x->Interval(Value(n),Infinity)))}
                  else {Some(precond)}
              }
              case (Infinity, Infinity) => {
                  Some(precond + (x->Interval(Value(n),Infinity)))
              }
            }
          case None => None
        }
    }
    case None => None
  }

  def abstractValNeg(expr:Bool, cond:Abstraction):Abstraction = cond match {
      case Some(precond) => expr match {
          case LessThan(x,Scalar(n)) => precond.get(x) match {
            case Some(v) => v match {
              case Interval(k,m) => (k,m) match {
                case (Value(a),Value(b)) => {
                  if(n<=a){Some(precond)}
                  else if(n<=b){Some(precond + (x->Interval(Value(n),Value(b))))}
                  else {None}
                }
                case (Infinity, Value(a)) => {
                    if(n<=a){Some(precond + (x->Interval(Value(n),Value(a))))}
                    else {None}
                }
                case (Value(a), Infinity) => {
                    if(a<n){Some(precond + (x->Interval(Value(n),Infinity)))}
                    else {Some(precond)}
                }
                case (Infinity, Infinity) => {
                    Some(precond + (x->Interval(Value(n),Infinity)))
                }
              }
            }
            case None => None
          }
          case GreaterThan(x,Scalar(n)) => precond.get(x) match {
              case Some(Interval(k,m)) => (k,m) match {
                case (Value(a),Value(b)) => {
                  if(b<=n){Some(precond)}
                  else if(a<=n){Some(precond + (x->Interval(Value(a),Value(n))))}
                  else {None}
                }
                case (Infinity, Value(a)) => {
                    if(n<a){Some(precond + (x->Interval(Infinity,Value(n))))}
                    else {Some(precond)}
                }
                case (Value(a), Infinity) => {
                    if(a<=n){Some(precond + (x->Interval(Value(a),Value(n))))}
                    else {None}
                }
                case (Infinity, Infinity) => {
                    Some(precond + (x->Interval(Infinity,Value(n))))
                }
              }
            case None => None
          }
      }
      case None => None
  }

  def widening(l:Abstraction, r:Abstraction):Abstraction = l match {
      case Some(lv) =>
      case None =>
  }

  def abs_iter(f:Abstraction => Abstraction, r:Abstraction):Abstraction = {
      val t = r;
      val newr = widening(r, f(r))
      if (newr == t){
          t
      } else {
          abs_iter(f, newr)
      }
  }

  def analysis(expr:Command, precond:Abstraction):Abstraction = expr match {
       case Skip => precond
       case Sequence(c1, c2) => analysis(c2, analysis(c1,precond))
       case Assign(x, e) => precond match {
           case Some(cond) => Some(cond + (x->abstractVal(e, cond)))
           case None => None
       }
       case Input(name) => precond match {
            case Some(cond) => Some(cond + (name->Interval(Infinity,Infinity)))
            case None => None
        }
       case IfElse(c,t,e) => abstractionUnion(analysis(t, abstractVal(c, precond)), analysis(e, abstractValNeg(c , precond)))
       case While(c, s) => abstractValNeg(c, abs_iter((x=>analysis(s, abstractVal(c, x))), precond))
   }
}
