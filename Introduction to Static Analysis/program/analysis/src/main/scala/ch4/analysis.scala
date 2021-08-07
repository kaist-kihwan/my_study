package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    // parse input program
    val expr = Program(str)
    // get label and fix it
    val new_expr, max_label = countLabel(expr, 0)
    // connect labels
    val labelnext = constructLabel(new_expr, 0, max_label)
    //boil out unneccesary information
    val labelmap = boilout(new_expr)
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

  def constructLabel(expr:Command, end:Int): LabelNext = expr match {
      case Skip(label) => Map[Int, Nexts](label -> NC_Next(end))
      case Sequence(label, c1, c2) => {
          constructLabel() ++ constructLabel() + (label -> NC_Next())
      }
      case Assign(label, n, e) => Map[Int, Nexts](label -> NC_Next(end))
      case Input(label, n) => Map[Int, Nexts](label -> NC_Next(end))
      case IfElse(label, c, t, e) => {
          constructLabel
      }
      case While(label, c, s) => {

      }
      case Goto(label, e) => Map[Int, Nexts]()
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

  def analysis(label:Int, precond:Abs_Memory, lm: LabelMap, ln: LabelNext): =  {

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
