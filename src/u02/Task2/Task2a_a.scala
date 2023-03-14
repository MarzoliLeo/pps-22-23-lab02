package u02.Task2

object Task2a_a extends App {
    def positive(x: Int) = x match
      case x if x >= 0 => "positive"
      case x if x < 0 =>  "negative"

    val positive2: Int => String = x => x match
      case x if x >= 0 => "positive "
      case x if x < 0 => "negative"

    println(positive(5))
    println(positive(-2))

    println(positive2(5))
    println(positive2(-2))
}
