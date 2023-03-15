package u02.Task3

object Task3_recursion extends App{
  def gcd(a: Int, b: Int): Int = b match
    case 0 => a
    case _ => gcd(b, a % b )

  def gcdTail(a: Int, b: Int): Int =
    @annotation.tailrec
    def loop(x: Int, y: Int): Int = y match
        case 0 => x
        case _ => loop(y, x % y)
    loop(a,b)

  println(gcd(36,24)) // 12
  println(gcdTail(36,24)) // 12
}
