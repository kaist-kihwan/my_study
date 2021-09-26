package StaticAnalysis

trait Mainframe {
    def error[T]:T = throw new Error(s"unknown error")

    // get two positive integer,
    // and return great common denominator of them.
    def gcd(n:Int, m:Int):Int = {

    }

    // get two positive integer,
    // and return least common multiplier of them.
    def lcm(n:Int, m:Int):Int = {

    }

    // get list of elements which has total order,
    // and return maximum.
    def maximum[T](list:List[T]):T = {
        val (h::t) = list
        if (t == Nil) {h}
        else {
            val m = maximum(t)
            if (h >= m) {h}
            else {m}
        }
    }

    // get list of elements which has total order,
    // and return minimum.
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
