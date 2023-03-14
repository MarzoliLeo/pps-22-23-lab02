package u02.Task2

object Task2b_composition extends App{
  def compose(f: Int => Int, g: Int => Int): Int => Int = { x => g(f(x)) }
  def compose2[A, B, C](f: B => C, g: A => B): A => C = { x => f(g(x)) }

  print(compose(_ - 1, _ * 2)(5))
  //Decommentando questa print ed eseguendo il codice l'IDE risponderà che le funzioni definite non appartengono ad "Any".
  //(print(compose2(_ * 2, _ - 2)(5))

}
