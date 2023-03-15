package u02.Task2

import u02.Optionals.Option

object AllTasks extends App {

    //Task 1, svolto da solo.

    def positive(x: Int) = x match
      case x if x >= 0 => "positive"
      case x if x < 0 =>  "negative"

    val positive2: Int => String = x => x match
      case x if x >= 0 => "positive "
      case x if x < 0 => "negative"

    println("Task part 2a_a: Stampa positive/negative con def.")
    println(positive(5))
    println(positive(-2))
    println("Task part 2a_a: Stampa positive/negative con val.")
    println(positive2(5))
    println(positive2(-2))

    //Task 2, svolto da solo.

    val empty: String => Boolean = _ == ""
    val notEmpty = neg(empty)

    def neg(f: String => Boolean): (s: String) => Boolean =
      s => !f(s)

    val neg2: (String => Boolean) => String => Boolean =
      f => s => !f(s)
    def neg3[A](f: A => Boolean): A => Boolean =
      (a: A) => !f(a)

    println("Task part 2a_b: Stampa dei valori di verità di neg come def. ")
    println(notEmpty("foo"))
    println(notEmpty(""))
    println(notEmpty("foo") && !notEmpty(""))

    val notEmpty2 = neg2(empty)
    println("Task part 2a_b: Stampa dei valori di verità di neg2 come lambda. ")
    println(notEmpty2("foo"))
    println(notEmpty2(""))
    println(notEmpty2("foo") && !notEmpty2(""))

    val notEmpty3 = neg3(empty)
    println("Task part 2a_b: Stampa dei valori di verità di neg3 come def generica. ")
    println(notEmpty3("foo"))
    println(notEmpty3(""))
    println(notEmpty3("foo") && !notEmpty3(""))

    //Task 3, svolto da solo.
    
    def compose(f: Int => Int, g: Int => Int): Int => Int = { x => g(f(x)) }
    def compose2[A, B, C](f: B => C, g: A => B): A => C = { x => f(g(x)) }

    println("Task part 2b - composition: stampa di composizione. ")
    println(compose(_ - 1, _ * 2)(5))
    /* Limitazione:
     * Decommentando questa print ed eseguendo il codice l'IDE risponderà che le funzioni definite non appartengono ad "Any".
       (println(compose2(_ * 2, _ - 2)(5))
     * */

    //Task 4, svolto da solo.

    val p1: Int => Int => Int => Boolean = { x => y => z => x <= y && x <= z && y == z }
    val p2: (Int, Int, Int) => Boolean = { (x, y, z) => x <= y && x <= z && y == z }
    def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && x <= z && y == z
    def p4(x: Int, y: Int, z: Int): Boolean = x <= y && x <= z && y == z

    println("Task part 2b - currying: stampa di lambda CON currying ")
    println(p1(5)(7)(7))
    println("Task part 2b - currying: stampa di lambda SENZA currying ")
    println(p2(5, 7, 7))
    println("Task part 2b - currying: stampa di metodo CON currying ")
    println(p3(5)(7)(7))
    println("Task part 2b - currying: stampa di  metodo SENZA currying  ")
    println(p4(5, 7, 7))

    //Task 5, svolto da solo.

    def gcd(a: Int, b: Int): Int = b match
      case 0 => a
      case _ => gcd(b, a % b)

    def gcdTail(a: Int, b: Int): Int =
      @annotation.tailrec
      def loop(x: Int, y: Int): Int = y match
        case 0 => x
        case _ => loop(y, x % y)
      loop(a, b)

    println("Task part 3: Ricorsione. ")
    println(gcd(36, 24)) // 12
    println("Task part 3: Ricorsione Tail. ")
    println(gcdTail(36, 24)) // 12

    //Task 6, svolto da solo.

    enum Shape:
      case Rectangle(width: Double, eight: Double, corner1: (Double, Double), corner2: (Double, Double))
      case Circle(radius: Double, center: (Double, Double))
      case Square(side: Double, corner1: (Double, Double), corner2: (Double, Double))

    object Shape:
      def getPerimeter(p: Shape): Double = p match
        case Shape.Rectangle(a, b, _, _) => 2 * a + 2 * b
        case Shape.Circle(r, _) => (2 * r) * 3.1418
        case Shape.Square(s, _, _) => 4 * s

      def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
        case Shape.Rectangle(_, _, c1, c2) => (point._1 > c1._1 && point._1 < c1._2) && (point._2 > c2._1 && point._2 < c2._2)
        case Shape.Circle(r, c) => val dx = point._1 - c._1
          val dy = point._2 - c._2
          val distance = math.sqrt(dx * dx + dy * dy)
          distance < r
        case Shape.Square(_, c1, c2) => (point._1 > c1._1 && point._1 < c1._2) && (point._2 > c2._1 && point._2 < c2._2)

    import Shape.*

    println("Task part 4: Stampa dei perimetri")
    println(getPerimeter(Circle(5.0, (1.0, 1.0)))) //31.418
    println(getPerimeter(Rectangle(5.0, 2.0, (7.3, 5.4), (3.2, 8.0)))) //14.0
    println(getPerimeter(Square(5.0, (7.3, 5.4), (3.2, 8.0)))) //20.0

    println("Task part 4: Stampa dei punti contenuti")
    println(contains(Circle(5.0, (10.0, 10.0)), point = (11.0, 11.0))) //true
    println(contains(Circle(5.0, (10.0, 10.0)), point = (5.0, 5.0))) //false
    println(contains(Rectangle(5.0, 2.0, (5.0, 10.0), (3.2, 8.0)), point = (7.3, 5.5))) //true
    println(contains(Rectangle(5.0, 2.0, (5.0, 10.0), (3.2, 8.0)), point = (5.0, 2.0))) //false
    println(contains(Square(5.0, (5.0, 10.0), (3.2, 8.0)), point = (5.8, 5.5))) //true
    println(contains(Square(5.0, (5.0, 10.0), (3.2, 8.0)), point = (3.1, 9.0))) //false

    //Task 7, svolto da solo.

    enum Option[A]:
      case Some(a: A)
      case None()

    object Option:
      def filter[A](option: Option[A])(predicate: A => Boolean): Option[A] = option match
        case Some(value) if predicate(value) => Some(value)
        case _ => None()

      def map[A, B](option: Option[A])(f: A => B): Option[B] = option match
        case Some(value) => Some(f(value))
        case _ => None()

      def fold[A, B](option: Option[A])(defaultValue: B)(f: A => B): B = option match
        case Some(value) => f(value)
        case _ => defaultValue

    import Option.*

    val s1: Option[Int] = Some(1)
    val s2: Option[Int] = Some(2)
    val s3: Option[Int] = None()

    println("Task part 5: Filter. ")
    println(filter(Some(5))(_ > 2)) // Some(5)
    println(filter(Some(5))(_ > 8)) // None
    println(filter(None[Int]())(_ > 2)) // None

    println("Task part 5: Map. ")
    println(map(Some(5))(_ > 2)) // Some(true)
    println(map(Some(5))(_ > 8)) // Some(false)
    println(map(None[Int]())(_ > 2)) // None

    println("Task part 5: Fold. ")
    println(fold(Some(5))(1)(_ + 1)) // 6
    println(fold(None[Int]())(1)(_ + 1)) // 1
}
