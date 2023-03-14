package u02.Task2

object Task2b_currying extends App {
    val p1: Int => Int => Int => Boolean = { x => y => z => x<=y && x<=z && y==z } //con currying.
    val p2: (Int, Int , Int) => Boolean = { (x, y, z) => x<=y && x<=z && y==z } //senza currying.
    def p3 (x: Int)(y: Int)(z:Int) : Boolean = x<=y && x<=z && y==z //con currying.
    def p4 ( x: Int , y: Int , z: Int): Boolean = x<=y && x<=z && y==z //senza currying.
    
    println(p1(5)(7)(7))
    println(p2(5,7,7))
    println(p3(5)(7)(7))
    println(p4(5,7,7))
}
