package StaticAnalysis

package object ch4 extends Chapter4 {

  def run(str:String, cond:String):Unit = {
    val expr = Program(str)
    val labelmap, max_label = labeling(expr, 0)
    printLabelMap(labelmap, max_label, 0)
    val labelstructure = connectLabel(labelmap, max_label, 0)
    val precond = Condition(cond)
    analysis(expr, precond, labelstructure, labelmap) match {
        case Some(v) => println("{%s}".format(abstractionToString(v, v.keys.toList).mkString(", ")))
        case None => println("bottom")
    }
  }

  def printLabelMap(labelmap: LabelMap, max_label, iter):Unit = {

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

  trait CommandLabel
  case object Skip_L extends CommandLabel
  case object Sequence_L extends CommandLabel
  case class Assign_L(name:String, expr: Expression) extends CommandLabel
  case class Input_L(name:String) extends CommandLabel
  case class IfElse_L(cond:Bool) extends CommandLabel
  case class While_L(cond: Bool) extends CommandLabel
  case class Goto_L(lab:Expression) extends CommandLabel

  type LabelMap = Map[Int, CommandLabel]

  def labeling(program: Command, alloc:Int): (LabelMap, Int) = program match {
      case Skip => (Map[Int, CommandLabel](alloc -> Skip_L), alloc + 1)
      case Sequence(c1, c2) => {
          val lm1, a1 = labeling(c1, alloc + 1)
          val lm2, a2 = labeling(c2, a1)
          (lm1 ++ lm2 + (alloc -> Sequence_L), a2)
      }
      case Assign(name, e) => (Map[Int, CommandLabel](alloc -> Assign_L(name, e)), alloc + 1)
      case Input(name) => (Map[Int, CommandLabel](alloc -> Input_L(name)), alloc + 1)
      case IfElse(cond, thenC, elseC) => {
          val lm1, a1 = labeling(thenC, alloc + 1)
          val lm2, a2 = labeling(elseC, a1)
          (lm1 ++ lm2 + (alloc -> IfElse_L(cond)), a2)
      }
      case While(cond, statement) => {
          val lm, a = labeling(statement, alloc + 1)
          (lm + (alloc -> While_L(cond)), a)
      }
      case Goto(l) => (Map[Int, CommandLabel](alloc -> Goto_L(l)), alloc + 1)
  }

  def analysis(expr:Command, precond:Abstraction, labelstructure:LabelStructure):Abstraction = expr match {
      case
  }

}
