package StaticAnalysis

trait Abstract_Domain extends ProgramExcerpt{

    type AbstractElement
    type Abstraction = Option[Map[String, AbstractElement]]
    def apply(str:String):Abstraction // parsing input string
    def update(abs:Abstraction, name:String, value:AbstractElement):Abstraction
    def abs_union(left:Abstraction, right:Abstraction):Abstraction
    def abs_joint(left:Abstraction, right:Abstraction):Abstraction
    def abs_filtering(bool:Bool, abs:Abstraction):Abstraction
    def abs_evaluate(expr:Expression, abs:Abstraction):AbstractElement

}

trait Widening {

    def run(init:Abstraction, func:(Abstraction => Abstraction)):Abstraction

}
