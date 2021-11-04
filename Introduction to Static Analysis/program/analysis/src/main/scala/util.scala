package StaticAnalysis

trait Mainframe {
    def error[T]:T = throw new Error(s"unknown error")

    // get two positive integer,
    // and return great common devisor of them.
    // return value must be positive.
    def gcd(n:Int, m:Int):Int = m match {
        case 0 => n
        case _ => gcd(m, n%m)
    }

    // get list of elements which has total order,
    // and return maximum.
    // def maximum[Int](list:List[Int]):Int = {
    //     val (h::t) = list
    //     if (t == Nil) {h}
    //     else {
    //         val m = maximum(t)
    //         if (h >= m) {h}
    //         else {m}
    //     }
    // }

    // // get list of elements which has total order,
    // // and return minimum.
    // def minimum[Int](list:List[Int]):Int = {
    //     val (h::t) = list
    //     if (t == Nil) {h}
    //     else {
    //         val m = maximum(t)
    //         if (h <= m) {h}
    //         else {m}
    //     }
    // }
}
