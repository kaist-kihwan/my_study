package StaticAnalysis

trait Abstract_Domain extends ProgramExcerpt{

    type AbstractElement
    type Abstraction = Option[Map[String, AbstractElement]]
    // parsing input string
    def apply(str:String):Abstraction
    // element to string
    def elementToString(abse:AbstractElement):String
    def toString(keyList:List[String], abs:Map[String, AbstractElement]):List[String] = keyList match {
        case h :: t => "%s->%s".format(h, elementToString(h)) :: toString(t, abs)
        case Nil => List[String]()
    }
    // basic operations of abstraction
    def update(abs:Abstraction, name:String, value:AbstractElement):Abstraction
    def union(left:Abstraction, right:Abstraction):Abstraction
    def joint(left:Abstraction, right:Abstraction):Abstraction
    def filtering(bool:Bool, abs:Abstraction):Abstraction
    def evaluate(expr:Expression, abs:Abstraction):AbstractElement
    def bottom:Abstraction = None
    def top:Abstraction

}

trait Widening {

    def run(
        init:Abstraction, func:(Abstraction => Abstraction), abs_dom:Abstract_Domain
    ):Abstraction

}
