package StaticAnalysis

trait Abstract_Domain extends ProgramExcerpt{

    type AbstractElement
    type Memory = Map[String, AbstractElement]
    type Abstraction = Option[Memory]
    // parsing input string
    def apply(str:String):Abstraction
    // element to string
    def elementToString(abse:AbstractElement):String
    def toString(keyList:List[String], abs:Memory):List[String] = keyList match {
        case h :: t => "%s->%s".format(h, elementToString(h)) :: toString(t, abs)
        case Nil => List[String]()
    }
    // basic operations of abstraction
    def update(abs:Abstraction, name:String, value:Option[AbstractElement]):Abstraction = abs match {
        case Some(v) => v + (name -> value)
        case None => None
    }
    def union(left:Abstraction, right:Abstraction):Abstraction = (left, right) match {
        case (Some(lv), Some(rv)) => Some(_union((lv.keySet ++ rv.keySet).toList, lv, rv))
        case (Some(lv), None) => left
        case (None, Some(rv)) => right
        case (None, None) => None
    }

    def _union(keyList:List[String], left:Memory, right:Memory):Memory = keyList match {
        case h :: t => _union(t, left, right) + (h -> ((left.get(h), right.get(h)) match {
            case (Some(lv), Some(rv)) => unionElement(lv, rv)
            case (Some(lv), None) => lv
            case (None, Some(rv)) => rv
        }))
        case Nil => Map()
    }
    def unionElement(left:AbstractElement, right:AbstractElement):AbstractElement
    def joint(left:Abstraction, right:Abstraction):Abstraction = (left, right) match {
        case (Some(lv), Some(rv)) => _joint((lv.keySet & rv.keySet).toList, lv, rv)
        case (_, None) | (None, _) => None
    }
    def _joint(keyList:List[String], left:Memory, right:Memory):Abstraction = keyList match {
        case h :: t => jointElement(left.get(h).get, right.get(h).get) match {
            case Some(v1) => _joint(t, left, right) match {
                case Some(v2) => Some(v2 + (h -> v1))
                case None => None
            }
            case None => None
        }
        case Nil => Some(Map())
    }
    def jointElement(left:AbstractElement, right:AbstractElement):Option[AbstractElement]
    def filtering(bool:Bool, abs:Abstraction):Abstraction = bool match {
        case LessThan(_) | GreaterThan(_) => abs match {
            case Some(mem) => filterElement(bool, abs) {

            }
            case None => None
        }
        case AndGate(left, right) => union(filtering(left), filtering)
        case OrGate(left, right) =>
        case True => abs
        case False => None
        case Random => abs
    }
    def filterElement(b:Bool, abse:AbstractElement):Option[AbstractElement]
    def evaluate(expr:Expression, abs:Abstraction):Option[AbstractElement]
    def bottom:Abstraction = None
    def topElement:AbstractionElement

}

trait Widening {

    def run(
        init:Abstraction, func:(Abstraction => Abstraction), abs_dom:Abstract_Domain
    ):Abstraction

}
