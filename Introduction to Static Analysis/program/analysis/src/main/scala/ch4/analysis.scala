package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    // parse input program
    val expr = Program(str)
    // get label and fix it
    val (new_expr, max_label) = countLabel(expr, 0)
    val start = getLabel(new_expr)
    // create empty labelmap and labelnext
    val labelmap = new Array[CommandLabel](max_label+1)
    val labelnext = new Array[(Int, Int)](max_label+1)
    // connect labels
    constructLabel(new_expr, labelnext, max_label)
    //boil out unneccesary information
    boilout(new_expr, labelmap)
    labelmap(max_label) = Endpoint_L
    // parse initial abstraction
    val precond = Condition(cond)
    // analyzing to find infinite union of F#
    val result = analysis(start, precond, labelmap, labelnext)
    println("{%s}".format(abstractionToString(result.keys.toList, result).mkString(", ")))
  }

  def abstractionToString(keylist:List[Int], abst:Abstraction):List[String] = keylist match {
      case h :: t => "(%d -> %s)".format(h, memoryToString(abst.get(h).get)) :: abstractionToString(t, abst)
      case Nil => List[String]()
  }

  def memoryToString(absm:Abs_Memory):String = absm match {
      case Some(abses) => "{%s}".format(elementToString(abses, abses.keys.toList).mkString(", "))
      case None => "bottom"
  }

  def elementToString(a:Abs_Els, keys:List[String]):List[String] = keys match {
    case h :: t => "%s->%s".format(h, intervalToString(a.get(h).get)) :: elementToString(a, t)
    case Nil => List[String]()
  }

  def intervalToString(i:Abs_Element):String = {
      val (a,b) = i
      (a,b) match {
          case (Val(n), Val(m)) => "[%d,%d]".format(n, m)
          case (Infinity, Val(n)) => "[-inf,%d]".format(n)
          case (Val(n),Infinity) => "[%d,inf]".format(n)
          case (Infinity,Infinity) => "[-inf,inf]"
      }
  }

  // set label in Command case classes, and find end label number
  def countLabel(expr:Command, alloc:Int): (Command, Int) = expr match {
      case Skip(_) => (Skip(alloc), alloc + 1)
      case Sequence(_, c1, c2) => {
          val (new_c1, a1) = countLabel(c1, alloc)
          val (new_c2, a2) = countLabel(c2, a1 + 1)
          (Sequence(a1, new_c1, new_c2), a2)
      }
      case Assign(_,n,e) => (Assign(alloc, n, e), alloc + 1)
      case Input(_,n) => (Input(alloc, n), alloc + 1)
      case IfElse(_, c, t, e) => {
          val (new_t, a1) = countLabel(t, alloc + 1)
          val (new_e, a2) = countLabel(e, a1)
          (IfElse(alloc, c, new_t, new_e), a2)
      }
      case While(_, c, s) => {
          val (new_s, new_a) = countLabel(s, alloc + 1)
          (While(alloc, c, new_s), new_a)
      }
      case Goto(_, e) => (Goto(alloc, e), alloc + 1)
  }

  def constructLabel(expr:Command, ln:LabelNext, end:Int):Unit = expr match {
      case Skip(label) => ln(label) = (end, -1)
      case Assign(label, _, _) => ln(label) = (end, -1)
      case Input(label, _) => ln(label) = (end, -1)
      case Sequence(label, c1, c2) => {
        constructLabel(c1, ln, getLabel(c2))
        constructLabel(c2, ln, end)
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
      val func = (abst:Abstraction) => {
          abs_union(abs_step(abst, lm, ln), label, precond)
      }
      repeating(worklist, lm.size, Map[Int, Abs_Memory](label -> precond), func)
  }

  // repeat widening abstractions
  def repeating(worklist:List[Int], maxlabel:Int, precond:Abstraction, func:Abstraction => Abstraction): Abstraction = {
      val aR = precond
      val aC = widening(precond, func(apply_worklist(precond, worklist)))
      val new_worklist = nextWorkList(aC, aR, List.range(0, maxlabel+1))
      if (worklist.size > 0) {repeating(new_worklist, maxlabel, aC, func)}
      else {aR}
  }

  def nextWorkList(aC:Abstraction, aR:Abstraction, list:List[Int]):List[Int] = list match {
      case h :: t => (aC.get(h), aR.get(h)) match {
          case (Some(aCv), Some(aRv)) => (aCv, aRv) match {
              case (Some(aCve), Some(aRve)) => {
                  if (memory_inclusion(aCve.keys.toList, aCve, aRve)) {nextWorkList(aC, aR, t)}
                  else {nextWorkList(aC, aR, t).appended(h)}
              }
              case (Some(aCve), None) => nextWorkList(aC, aR, t).appended(h)
              case (None, Some(aRve)) => nextWorkList(aC, aR, t)
              case (None, None) => nextWorkList(aC, aR, t)
          }
          case (Some(aCv), None) => nextWorkList(aC, aR, t).appended(h)
          case (None, Some(aRv)) => nextWorkList(aC, aR, t)
          case (None, None) => nextWorkList(aC, aR, t)
      }
      case Nil => List[Int]()
  }

  def apply_worklist(cond:Abstraction, worklist:List[Int]):Abstraction = worklist match {
      case h :: t => apply_worklist(cond, t) + (cond.get(h) match {
          case Some(absm) => (h -> absm)
          case None => (h -> None)
      })
      case Nil => Map[Int, Abs_Memory]()
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
  def widening(left:Abstraction, right:Abstraction):Abstraction = {
      widening_helper((left.keySet union right.keySet).toList, left, right)
  }

  def widening_helper(keylist:List[Int], left:Abstraction, right:Abstraction):Abstraction = keylist match {
      case h :: t => widening_helper(t, left, right) ++ ((left.get(h), right.get(h)) match {
          case (Some(lm), Some(rm)) => (lm, rm) match {
              case (Some(les), Some(res)) => Map[Int, Abs_Memory](
                  h -> Some(widening_helper_helper((les.keySet union res.keySet).toList, les, res))
              )
              case (Some(v), None) => Map[Int, Abs_Memory](h -> Some(v))
              case (None, Some(v)) => Map[Int, Abs_Memory](h -> Some(v))
              case (None, None) => Map[Int, Abs_Memory]()
          }
          case (Some(v), None) => Map[Int, Abs_Memory](h -> v)
          case (None, Some(v)) => Map[Int, Abs_Memory](h -> v)
          case (None, None) => Map[Int, Abs_Memory]()
      })
      case Nil => Map[Int, Abs_Memory]()
  }

  def widening_helper_helper(keylist:List[String], left:Abs_Els, right:Abs_Els):Abs_Els = keylist match {
      case h :: t => widening_helper_helper(t, left, right) + (h -> ((left.get(h), right.get(h)) match {
          case (Some(lv), Some(rv)) => {
              val ((lv1, lv2),(rv1, rv2)) = (lv, rv)
              val new1 = (lv1, rv1) match {
                  case (Val(lv1v), Val(rv1v)) => {
                      if (lv1v <= rv1v) {Val(lv1v)}
                      else {Infinity}
                  }
                  case (Val(_), Infinity) => Infinity
                  case (Infinity, _) => Infinity
              }
              val new2 = (lv2, rv2) match {
                  case (Val(lv2v), Val(rv2v)) => {
                      if (lv2v >= rv2v) {Val(lv2v)}
                      else {Infinity}
                  }
                  case (Val(_), Infinity) => Infinity
                  case (Infinity, _) => Infinity
              }
              (new1, new2)
          }
          case (Some(v), None) => v
          case (None, Some(v)) => v
          case (None, None) => error
      }))
      case Nil => Map[String, Abs_Element]()
  }

  def abs_step(abst:Abstraction, lm:LabelMap, ln:LabelNext):Abstraction = {
      val ir1 = abs_power_semantic(abst, abst.keys.toList, lm, ln)
      val ir2 = sortAndUnion(ir1, abst)
      ir2
  }

  // calc next step of given abs state
  def abs_power_semantic(abst:Abstraction, keylist:List[Int], lm:LabelMap, ln:LabelNext):List[State] = keylist match {
      case h::t => {
          abs_power_semantic(abst, t, lm, ln) ++ abs_semantic((h, abst.get(h).get), lm, ln)
      }
      case Nil => List[State]()
  }

  def sortAndUnion(list:List[State], abs:Abstraction):Abstraction = list match {
      case h :: t => {
          val (l,m) = h
          abs_union(sortAndUnion(t, abs), l, m)
      }
      case Nil => abs
  }

  def abs_union(abs:Abstraction, l:Int, m:Abs_Memory):Abstraction = {
      abs.get(l) match {
          case None => abs + (l -> m)
          case Some(m0) => abs + (l -> memory_union(m0, m))
      }
  }

  def memory_union(left:Abs_Memory, right:Abs_Memory):Abs_Memory = (left, right) match {
      case (Some(lv), Some(rv)) => Some(memory_union_helper((lv.keySet union rv.keySet).toList, lv, rv))
      case (Some(v), None) => Some(v)
      case (None, Some(v)) => Some(v)
      case (None, None) => None
  }

  def memory_union_helper(keylist:List[String], left:Abs_Els, right:Abs_Els):Abs_Els = keylist match {
      case h :: t => (left.get(h), right.get(h)) match {
          case (Some(lv), Some(rv)) => memory_union_helper(t, left, right) + (h -> interval_union(lv, rv))
          case (Some(v), None) => memory_union_helper(t, left, right) + (h -> v)
          case (None, Some(v)) => memory_union_helper(t, left, right) + (h -> v)
          case (None,None) => error
      }
      case Nil => Map[String, Abs_Element]()
  }

  def interval_union(left:Abs_Element, right:Abs_Element):Abs_Element = {
      val ((lv1, lv2),(rv1, rv2)) = (left, right)
      val new1 = (lv1, rv1) match {
          case (Val(lv1v), Val(rv1v)) => {
              if (lv1v < rv1v) {Val(lv1v)}
              else {Val(rv1v)}
          }
          case _ => Infinity
      }
      val new2 = (lv2, rv2) match {
          case (Val(lv2v), Val(rv2v)) => {
              if (lv2v < rv2v) {Val(rv2v)}
              else {Val(lv2v)}
          }
          case _ => Infinity
      }
      (new1, new2)
  }

  def abs_semantic(s:State, lm:LabelMap, ln:LabelNext): List[State] = {
      val (label, absm) = s
      lm(label) match {
        case Skip_L => List[State]((ln(label)._1, absm))
        case Sequence_L => List[State]((ln(label)._1, absm))
        case Assign_L(x, e) => evaluate(absm, e) match {
            case Some(v) => List[State]((ln(label)._1, update(absm, x, v)))
            case None => List[State]()
        }
        case Input_L(x) => List[State]((ln(label)._1, update(absm, x, (Infinity, Infinity))))
        case IfElse_L(c) => {
            List[State]((ln(label)._1, abs_filter(c, absm)), (ln(label)._2, abs_filter(negate(c), absm)))
        }
        case While_L(c) => {
            List[State]((ln(label)._1, abs_filter(c, absm)), (ln(label)._2, abs_filter(negate(c), absm)))
        }
        case Goto_L(e) => evaluate(absm, e) match {
            case Some(v) => {
                abs_semantic_helper(
                    elementToList(
                        joint(
                            v, (Val(0), Val(lm.size))
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
           abs_semantic_helper(t, absm).appended((h, absm))
       }
       case Nil => List[State]()
  }

  def joint(left:Abs_Element, right:Abs_Element):Abs_Element = {
      val ((l1,l2),(r1,r2)) = (left, right)
      val new1 = (l1, r1) match {
          case (Val(l1n), Val(r1n)) => if(l1n < r1n) {Val(r1n)} else {Val(l1n)}
          case (Infinity, Val(v)) => Val(v)
          case (Val(v), Infinity) => Val(v)
          case (Infinity, Infinity) => Infinity
      }
      val new2 = (l2, r2) match {
          case (Val(l2n), Val(r2n)) => if(l2n < r2n) {Val(l2n)} else {Val(r2n)}
          case (Infinity, Val(v)) => Val(v)
          case (Val(v), Infinity) => Val(v)
          case (Infinity, Infinity) => Infinity
      }
      (new1, new2)
  }

  def elementToList(ele:Abs_Element):List[Int] = ele match {
      case (Val(a), Val(b)) => List.range(a, b+1)
      case _ => error
  }

  def negate(c: Bool):Bool = c match {
      case LessThan(x, n) => GreaterThan(x, n)
      case GreaterThan(x, n) => LessThan(x, n)
      case True => False
      case False => True
  }

  def abs_filter(c:Bool, absm:Abs_Memory):Abs_Memory = absm match {
      case Some(abses) => c match {
          case LessThan(x, ex) => evaluate(absm, ex) match {
              case Some(abse) => absm match {
                  case Some(abses) => abses.get(x) match {
                      case Some(original) => {
                          val (_, v) = abse
                          Some(abses + (x -> joint(original, (Infinity, v))))
                      }
                      case None => None
                  }
                  case None => None
              }
              case None => None
          }
          case GreaterThan(x, ex) => evaluate(absm, ex) match {
              case Some(abse) => absm match {
                  case Some(abses) => abses.get(x) match {
                      case Some(original) => {
                          val (v, _) = abse
                          Some(abses + (x -> joint(original, (v, Infinity))))
                      }
                      case None => None
                  }
                  case None => None
              }
              case None => None
          }
          case True => absm
          case False => None
      }
      case None => None // bottom
  }

  // update abstract memory with x and e
  def update(absm:Abs_Memory, x:String, alpha:Abs_Element):Abs_Memory = absm match {
      case Some(abses) => Some(abses + (x -> alpha))
      case None => Some(Map[String, Abs_Element](x -> alpha))
  }

  def evaluate(absm:Abs_Memory, e:Expression):Option[Abs_Element] = e match {
      case Scalar(n) => Some(Val(n),Val(n))
      case Variable(x) => absm match {
          case Some(absmv) => absmv.get(x)
          case None => None
      }
      case Plus(l, r) => (evaluate(absm, l), evaluate(absm, r)) match {
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
      case Minus(l, r) => (evaluate(absm, l), evaluate(absm, r)) match {
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

}
