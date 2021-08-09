package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    // parse input program
    val expr = Program(str)
    // get label and fix it
    val new_expr, max_label = countLabel(expr, 0)
    // create empty labelmap and labelnext
    val empty_lm = new Array[CommandLabel](max_label+1)
    val empty_ln = new Array[(Int, Int)](max_label+1)
    // connect labels
    val labelnext = constructLabel(new_expr, empty_ln, max_label) + (max_label -> Halt)
    //boil out unneccesary information
    val labelmap = boilout(new_expr, empty_lm)
    // parse initial abstraction
    val precond = Condition(cond)
    // analyzing to find infinite union of F#
    analysis(0, precond, labelmap, labelnext) match {
        // print result
        case Some(v) => println("{%s}".format(abstractionToString(v, v.keys.toList).mkString(", ")))
        case None => println("bottom")
    }
  }

  // set label in Command case classes, and find end label number
  def countLabel(expr:Command, alloc:Int): (Command, Int) = expr match {
      case Skip(_) => (Skip(alloc), alloc + 1)
      case Sequence(_, c1, c2) => {
          val new_c1, a1 = countLabel(c1, alloc)
          val new_c2, a2 = countLabel(c2, a1 + 1)
          (Sequence(a1, new_c1, new_c2), a2)
      }
      case Assign(_,n,e) => (Assign(alloc, n, e), alloc + 1)
      case Input(_,n) => (Input(alloc, n), alloc + 1)
      case IfElse(_, c, t, e) => {
          val new_t, a1 = countLabel(t, alloc + 1)
          val new_e, a2 = countLabel(a1 + 1)
          (IfElse(alloc, c, new_t, new_e), a2)
      }
      case While(_, c, s) => {
          val new_s, new_a = countLabel(s, alloc + 1)
          (While(alloc, c, new_s), new_a)
      }
      case Goto(_, e) => (Goto(alloc, e), alloc + 1)
  }

  def constructLabel(expr:Command, ln:LabelNext, end:Int):Unit = expr match {
      case Skip(label) | Assign(label, n, e) | Input(label, n) => {
        ln(label) = (end, -1)
      }
      case Sequence(label, c1, c2) => {
        constructLabel(c1, ln, getLabel(c2))
        constructLabel(c2, ln, end))
        ln(label) = (getLabel(c1), -1)
      }
      case IfElse(label, c, t, e) => {
          constructLabel(t, ln, end)
          constructLabel(e, ln, end)
          ln(label) = (getLabel(t), getLabel(e))
      }
      case While(label, c, s) => {
          constructLabel(s, ln, label)
          ln(label) = (getLabel(s), end)
      }
      case Goto(label, e) => ()
  }

  def boilout(expr:Command, lm:LabelMap):Unit  = expr match {
      case Skip(l) => lm(l) = Skip_L
      case Sequence(l, c1, c2) => {
         lm(l) = Sequence_L
         boilout(c1, lm)
         boilout(c2, lm)
      }
      case Assign(l, n, e) => {
        lm(l) = Assign_L(n, e)
      }
      case Input(l, n) => {
        lm(l) = Input_L(n)
      }
      case IfElse(l, c, t, e) => {
        lm(l) = While_L(c)
        boilout(t, lm)
        boilout(e, lm)
      }
      case While(l, c, s) => {
        lm(l) = While_L(c)
        boilout(s, lm)
      }
      case Goto(l, e) => {
        lm(l) = Goto_L(e)
      }
  }

  def getLabel(command:Command): Int = command match {
      case Skip(l) => l
      case Sequence(l, _, _) => l
      case Assign(l, _, _) => l
      case Input(l, _) => l
      case IfElse(l, _, _, _) => l
      case While(l, _, _) => l
      case Goto(l, _) => l
  }

  def analysis(label:Int, precond:Abs_Memory, lm: LabelMap, ln: LabelNext):Abstraction = {
      // work list algorithm
      val worklist = List.range(0, lm.size)
      val func = () => {}
      repeating(worklist, lm.size, Map[Int, Abs_Memory](label -> precond), func)
  }

  // repeat widening abstractions
  def repeating(worklist:List[Int], maxlabel:Int, precond:Abstraction, func:Abstraction => Abstraction): Abstraction = {
      val aR = precond
      val aC = widening(precond, apply_worklist(precond, worklist))
      val new_worklist = List.range(0, maxlabel)
      if (abs_inclusion(aC.keys.toList, aC, aR)) {repeating(new_worklist, maxlabel, aC, func)}
      else {aR}
  }

  def apply_worklist(cond:Abstraction, worklist:List[Int]):Abstraction = worklist match {
      case h :: t =>
      case Nil =>
  }

  def abs_inclusion(keylist:List[Int], left:Abstraction, right:Abstraction):Boolean = keylist match {
      case h :: t => right.get(h) match {
          case Some(absm) => left.get(h).get match {
              case Some(labses) => absm match {
                  case Some(rabses) => {
                      if (memory_inclusion(labses.keys.toList, labses, rabses)){
                          abs_inclusion(t, left, right)
                      }
                      else {false}
                  }
                  case None => false // right is bottom, but left is not bottom
              }
              case None => abs_inclusion(t, left, right) // left is bottom
          }
          case None => false // no element
      }
      case Nil => true // finish recursion
  }

  def memory_inclusion(keylist:List[String], left:Abs_Els, right:Abs_Els):Boolean = keylist match {
      case h :: t => right.get(h) match {
          case Some(abse) => {
              if(interval_inclusion(left.get(h).get, abse)) {memory_inclusion(t, left, right)}
              else {false}
          }
          case None => false // no element
      }
      case Nil => true
  }

  def interval_inclusion(left:Abs_Element, right:Abs_Element):Boolean = {
      val ((lv1, lv2),(rv1, rv2)) = (left, right)
      val b1 = (lv1, rv1) match {
          case (Val(lv1v),Val(rv1v)) => (lv1v <= rv1v)
          case (Infinity, Val(_)) => false
          case (Infinity, Infinity) | (Val(_), Infinity) => true
      }
      val b2 = (lv2, rv2) match {
          case (Val(lv2v), Val(rv2v)) => (lv2v >= rv2v)
          case (Infinity, Val(_)) => false
          case (Infinity, Infinity) | (Val(_), Infinity) => true
      }
      (b1 && b2)
  }

  // use widening to find least fixpoint of F#
  def widening(left:Abstraction, right:Abstraction): = (left, right) match {
      case () => 
  }

  // calc next step of given abs state
  def abs_step(abst:Abstraction, keylist:List[Int], lm:LabelMap, ln:LabelNext):Abstraction = {
      val keylist match {
        case h::t => abst.get(h) match {
            case None =>
            case Some(absm) => abs_semantic((h, absm), lm, ln) ++
        }
        case Nil =>
      }

  }

  // def abs_union(left: Abstraction, right:Abstraction):Abstraction = {
  //     abs_union_helper((left.toSet union rigth.toSet).toList, left, right)
  // }
  //
  // def abs_union_helper(keylist:List[Int], left:Abstraction, right:Abstraction):Abstraction = keylist match {
  //     case h :: t => (left.get(h), right.get(h)) match {
  //         case (Some(lv), Some(rv)) => {
  //
  //         }
  //         case (None, Some(v)) | (Some(v), None)=> {
  //             Map[Int,Abs_Memory](h -> v)
  //         }
  //         case (None, None) => Map[Int,Abs_Memory]()
  //     }
  //     case Nil => Map[Int,Abs_Memory]()
  // }
  //
  // def interval_union(): = {
  //
  // }

  def abs_semantic(s:State, lm:LabelMap, ln:LabelNext): List[State] = {
      val label, absm = s
      lm(label) match {
        case Skip_L | Sequence_L => List[State]((ln(label)(0), absm))
        case Assign_L(x, e) => evaluate(absm, e) match {
            case Some(v) => List[State]((ln(label)(0), update(absm, x, v)))
            case None => List[State]()
        }
        case Input_L(x) => List[State]((ln(label)(0), update(absm, x)))
        case IfElse_L(c) | While_L(c) => {
            List[State]((ln(label)(0), abs_filter(c, absm)), (ln(label)(1), abs_filter(negate(c), absm)))
        }
        case Goto_L(e) => evaluate(absm, label) match {
            case Some(v) => {
                abs_semantic_helper(
                    elementToList(
                        joint(
                            v, (Value(0), Value(lm.size))
                        )
                    ), absm
                )
            }
            case None => List[State]()
        }
        case Endpoint_L => List[State]()
      }
  }

  def abs_semantic_helper(labels:List[Int], absm:Abs_Memory):List[State] = labels match {
       case h :: t => {
           recursive_semantic(t, absm).appended((h, absm))
       }
       case Nil => List[State]()
  }

  def joint(left:Abs_Element, right:Abs_Element):Abs_Element = {
      val ((l1,l2),(r1,r2)) = (left, right)
      val new1 = (l1, r1) match {
          case (Val(l1n), Val(r1n)) => if(l1n < r1n) {r1n} else {l1n}
          case (Infinity, Val(v)) | (Val(v), Infinity) => Val(v)
          case (Infinity, Infinity) => Infinity
      }
      val new2 = (l2, r2) match {
          case (Val(l2n), Val(r2n)) => if(l2n < r2n) {l2n} else {r2n}
          case (Infinity, Val(v)) | (Val(v), Infinity) => Val(v)
          case (Infinity, Infinity) => Infinity
      }
      (new1, new2)
  }

  def elementToList(ele:Abs_Element):List[Int] = ele match {
      case (Val(a), Val(b)) => List.range(a, b+1)
      case _ => error
  }

  def negate(c: Bool):Bool = c match {
      case LessThan(l, r) => GreaterThan(l, r)
      case GreaterThan(l, r) => LessThan(l, r)
      case True => False
      case False => True
  }

  def abs_filter(c:Bool, absm:Abs_Memory):Abs_Memory = c match {

  }

  // update abstract memory with x and e
  def update(absm:Abs_Memory, x:String, alpha:Abs_Element):Abs_Memory = absm.get(x) match {
      case Some(v) =>
      case None =>
  }

  def evaluate(absm:Abs_Memory, e:Expression):Option[Abs_Element] = e match {
      case Scalar(n) => Some(Val(n),Val(n))
      case Variable(x) => absm match {
          case Some(absmv) => absmv.get(x)
          case None => None
      }
      case Plus(l, r) => (evaluate(absm, l), evaluate(abs, r)) match {
          case (Some(lv), Some(rv)) => {
              val ((lv1, lv2), (rv1, rv2)) = (lv, rv)
              val new1 = (lv1, rv1) match {
                  case (Val(lv1v), Val(rv1v)) => Val(lv1v + rv1v)
                  case _ => Infinity
              }
              val new2 = (lv2, rv2) match {
                  case (Val(lv2v), Val(rv2v)) => Val(lv2v + rv2v)
                  case _ => Infinity
              }
              Some((new1, new2))
          }
          case _ => None
      }
      case Minus(l, r) => (evaluate(absm, l), evaluate(abs, r)) match {
          case (Some(lv), Some(rv)) => {
              val ((lv1, lv2), (rv1, rv2)) = (lv, rv)
              val new1 = (lv1, rv2) match {
                  case (Val(lv1v), Val(rv2v)) => Val(lv1v - rv2v)
                  case _ => Infinity
              }
              val new2 = (lv2, rv1) match {
                  case (Val(lv2v), Val(rv1v)) => Val(lv2v - rv1v)
                  case _ => Infinity
              }
              Some((new1, new2))
          }
          case _ => None
      }
  }

  def abstractionToString(a:Abstract, keys:List[String]):List[String] = keys match {
    case head::tail => "%s->%s".format(head, elementToString(a.get(head).get)) :: abstractionToString(a, tail)
    case Nil => List[String]()
  }

  def elementToString(i:AbstractElement):String = i match {
      case Interval(a,b) => (a,b) match {
          case (Val(n), Val(m)) => "[%d,%d]".format(n, m)
          case (Infinity, Val(n)) => "[-inf,%d]".format(n)
          case (Val(n),Infinity) => "[%d,inf]".format(n)
          case (Infinity,Infinity) => "[-inf,inf]"
      }
  }

}
