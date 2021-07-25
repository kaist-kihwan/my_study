package StaticAnalysis

package object ch4 extends Chapter4 {

  type LabelMap = Map[Int, Int]

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

  def analysis(expr:Command, precond:Abstraction, labelnext:LabelMap, labelnexttrue:LabelMap, labelnextfalse:LabelMap):Abstraction = expr match {
      case
  }

}
