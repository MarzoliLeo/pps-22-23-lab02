package u02.Task4

object Task4_shape extends App {
  enum Shape:
    case Rectangle( width:Double, eight:Double, corner1:(Double, Double), corner2:(Double, Double) )
    case Circle( radius:Double , center:(Double, Double))
    case Square( side:Double, corner1:(Double, Double), corner2:(Double, Double))

  object Shape:
    def getArea(p: Shape): Double = p match
      case Shape.Rectangle(a, b, _, _) => a*b
      case Shape.Circle(r, _) => (r*r) * 3.1418
      case Shape.Square(s, _, _) => s*s

    def getPerimeter(p: Shape): Double = p match
      case Shape.Rectangle(a, b, _, _) => 2*a + 2*b
      case Shape.Circle(r, _) => (2*r) * 3.1418
      case Shape.Square(s, _, _) => 4*s

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Shape.Rectangle(_, _, c1, c2) => (point._1 > c1._1 && point._1 < c1._2) && (point._2 > c2._1 && point._2 < c2._2)
      case Shape.Circle(r, c) => val dx = point._1 - c._1
                                 val dy = point._2 - c._2
                                 val distance = math.sqrt(dx * dx + dy * dy)
                                 distance < r
      case Shape.Square(_, c1, c2) => (point._1 > c1._1 && point._1 < c1._2) && (point._2 > c2._1 && point._2 < c2._2)

  import Shape.*

  println("Stampa dei perimetri")
  println(getPerimeter(Circle(5.0,(1.0,1.0)))) //31.418
  println(getPerimeter(Rectangle(5.0,2.0,(7.3,5.4),(3.2,8.0)))) //14.0
  println(getPerimeter(Square(5.0,(7.3,5.4),(3.2,8.0)))) //20.0

  println("Stampa dei punti contenuti")
  println(contains(Circle(5.0, (10.0, 10.0)), point = (11.0,11.0))) //true
  println(contains(Circle(5.0, (10.0, 10.0)), point = (5.0,5.0))) //false
  println(contains(Rectangle(5.0, 2.0, (5.0, 10.0), (3.2, 8.0)), point = (7.3,5.5))) //true
  println(contains(Rectangle(5.0, 2.0, (5.0, 10.0), (3.2, 8.0)), point = (5.0,2.0))) //false
  println(contains(Square(5.0, (5.0, 10.0), (3.2, 8.0)), point = (5.8, 5.5))) //true
  println(contains(Square(5.0, (5.0, 10.0), (3.2, 8.0)), point = (3.1, 9.0))) //false
}
