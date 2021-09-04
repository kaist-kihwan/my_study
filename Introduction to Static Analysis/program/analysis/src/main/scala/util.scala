package StaticAnalysis

trait Mainframe {
    def error[T]:T = throw new Error(s"unknown error")

    // useful helper functions
    def gcd(n:Int, m:Int):Int = {

    }

    def lcm(n:Int, m:Int):Int = {
        
    }

    def maximum[T](list:List[T]):T = {
        val (h::t) = list
        if (t == Nil) {h}
        else {
            val m = maximum(t)
            if (h >= m) {h}
            else {m}
        }
    }

    def minimum[T](list:List[T]):T = {
        val (h::t) = list
        if (t == Nil) {h}
        else {
            val m = maximum(t)
            if (h <= m) {h}
            else {m}
        }
    }
}
