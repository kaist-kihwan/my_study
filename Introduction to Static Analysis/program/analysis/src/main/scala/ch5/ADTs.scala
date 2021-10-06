package StaticAnalysis
// for encapsulization of code

trait Abstract_Domain extends ProgramExcerpt{

    type AbstractElement
    type Memory = Map[String, AbstractElement]
    type Abstraction = Option[Memory]
    // parsing input string
    def apply(str:String):Abstraction
    // element to string
    def elementToString(abse:AbstractElement):String

    def toString(keyList:List[String], abs:Memory):List[String] = keyList match {
        case h :: t => "%s->%s".format(h, elementToString(abs.get(h).get)) :: toString(t, abs)
        case Nil => List[String]()
    }
    // basic operations of abstraction
    def update(abs:Abstraction, name:String, value:AbstractElement):Abstraction = abs match {
        case Some(v) => Some(v + (name -> value))
        case None => None
    }

    def union(left:Abstraction, right:Abstraction):Abstraction = (left, right) match {
        case (Some(lv), Some(rv)) => Some(_union((lv.keySet ++ rv.keySet).toList, lv, rv))
        case (Some(_), None) => left
        case (None, Some(_)) => right
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

    // return abstraction mask for greaterThan or lessThan. Other Bool is unacceptable.
    def constraintToElement(option:Bool):AbstractElement

    def filtering(bool:Bool, abs:Abstraction):Abstraction = abs match {
        case Some(mem) => bool match {
            case LessThan(name, cons) => mem.get(name) match {
                case Some(v) => jointElement( v, constraintToElement(bool) ) match {
                    case Some(vv) => Some(mem + (name -> vv))
                    case None => None
                }
                case None => None
            }
            case GreaterThan(name, cons) => mem.get(name) match {
                case Some(v) => jointElement( v, constraintToElement(bool) ) match {
                    case Some(vv) => Some(mem + (name -> vv))
                    case None => None
                }
                case None => None
            }
            case AndGate(left, right) => joint(filtering(left, abs), filtering(right, abs))
            case OrGate(left, right) => union(filtering(left, abs), filtering(right, abs))
            case True => abs
            case False => None
            case Random => abs
        }
        case None => None
    }

    def ele_inclusion(left:AbstractElement, right:AbstractElement):Boolean

    def abs_inclusion(left:Abstraction, right:Abstraction):Boolean = left match {
        case Some(lm) => right match {
            case None => false
            case Some(rm) => inclusion_helper(rm.keys.toList, lm, rm)
        }
        case None => true
    }

    def inclusion_helper(keyList:List[String], lm:Memory, rm:Memory):Boolean = keyList match {
        case h :: t => lm.get(h) match {
            case Some(v) => {
                if (ele_inclusion(v, rm.get(h).get)) {inclusion_helper(t, lm, rm)}
                else {false}
            }
            case None => false
        }
        case Nil => true
    }

    def evaluate(expr:Expression, abs:Abstraction):Option[AbstractElement]

    def topElement:AbstractElement

    trait Widening {
        def run(
            init:Abstraction, func:(Abstraction => Abstraction),
        ):Abstraction
    }
}
