package StaticAnalysis

trait Mainframe {
    def error[T]:T = throw new Error(s"unknown error")
}
