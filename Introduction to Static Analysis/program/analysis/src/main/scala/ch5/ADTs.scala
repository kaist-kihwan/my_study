package StaticAnalysis

trait Abstract_Domain extends ProgramExcerpt{

    type AbstractElement
    def getElement:AbstractElement
    def abs_union
    def abs_joint
    def abs_filtering
    def abs_evaluate

}

trait Widening {

    def

}
