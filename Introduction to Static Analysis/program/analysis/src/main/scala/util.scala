package StaticAnalysis

trait Mainframe {
    def error[T]:T = throw new Error(s"unknown error")

    // useful helper functions
    def gcd(n:Int, m:Int):Int = {
        
    }
}
