package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    val expr = Program(str)
    val labelstructure, _ = labeling(expr, Map[Int, Nexts](1->Termination), 0, 1, 2)
    val precond = Condition(cond)
    analysis(expr, precond, labelstructure) match {
        case Some(v) => println("{%s}".format(abstractionToString(v, v.keys.toList).mkString(", ")))
        case None => println("bottom")
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

  def labeling(expr:Command, result:LabelStructure, start:Int, end:Int, alloc:Int): (LabelStructure, alloc) = expr match {
    case Skip => (result + (start -> Non_conditional(alloc))
                         + (alloc -> Nexts(end)),
                         alloc + 1)
    case Sequence(c1, c2) => (result + )
    case Assign(name, e) =>
    case Input(name) =>
    case IfElse(cond, thenC, elseC) =>
    case While(cond, st) =>
    case Goto(lab) =>
  }

  def analysis(expr:Command, precond:Abstraction, labelstructure:LabelStructure):Abstraction = expr match {
      case
  }

}
