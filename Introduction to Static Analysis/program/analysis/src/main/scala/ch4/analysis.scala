package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    // parse input program
    val expr = Program(str)
    // get label and fix it
    val new_expr, max_label = countLabel(expr, 0)
    // create empty labelmap and labelnext
    val empty_ln =
    val empty_lm =
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
      val abst = precond match {
        case
      }
      repeating(worklist, abst, func)
  }

  // repeat widening abstractions
  def repeating(worklist:List[Int], precond:Abstraction, func:Abstraction => Abstraction): Abstraction = {
      widening()
  }

  // use widening to find least fixpoint of F#
  def widening(left:Abstraction, right:Abstraction): = (left, right) match {
      case () =>
  }

  // calc next step of given abs state
  def abs_step(abst:Abstraction, keylist:List[Int], lm:LabelMap, ln:LabelNext):Abstraction = keylist match {
      case h::t => abst.get(h) match {
        case None =>
        case Some(absm) => abs_semantic((h, absm), lm, ln)
      }
      case Nil =>
  }

  def abs_semantic(s:State, lm:LabelMap, ln:LabelNext): State = {
      val label, absm = s
      lm(label) match {
        case Skip_L => (ln(label)(0), absm)
        case Sequence_L => (ln(label)(0), absm)
        case Assign_L(x, e) => (ln(label)(0)), update(absm, x, e)
        case Input_L =>
        case IfElse_L =>
        case While_L =>
        case Goto_L =>
      }

      Map[Int, Abs_Memory]()
  }

  // update abstract memory with x and e
  def update(absm:Abs_Memory, x:String, alpha:Abs_Element):Abs_Memory = absm.get(x) match {
      case Some(v) =>
      case None =>
  }

  def evaluate(absm:Abs_Memory, e:Expression):Abs_Element = e match {
      case
      case
  }

  def abs_inclusion(left:Abstraction, right:Abstraction):Boolean = {

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
