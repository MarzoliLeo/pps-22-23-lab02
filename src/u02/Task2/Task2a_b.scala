package u02.Task2

object Task2a_b extends App {
  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = neg(empty) // which type of notEmpty?

  //Ricordarsi che f identifica la funzione, s invece identifica la stringa su cui operare
  //il valore di ritorno è sempre quello posto dopo l'ultima freccia.
  def neg(f: String => Boolean) : (s : String) => Boolean  =
    s => !f(s)

  val neg2: (String => Boolean) => String => Boolean =
    f => s => !f(s)

  //Ricordati che ciò che non definisci nella firma del metodo, va definito a sinistra di => nel corpo per usarlo.
  // in (a: A) potresti anche solo mettere "a" senza ":A" perché c'è inferenza dei tipi.
  def neg3[A] (f: A => Boolean): A => Boolean =
    (a : A) => !f(a)

  println("Stampa dei valori di verità di neg: ")
  println(notEmpty("foo"))
  println(notEmpty(""))
  println(notEmpty("foo") && !notEmpty(""))

  val notEmpty2 = neg2(empty)
  println("Stampa dei valori di verità di neg2: ")
  println(notEmpty2("foo"))
  println(notEmpty2(""))
  println(notEmpty2("foo") && !notEmpty2(""))

  val notEmpty3 = neg3(empty)
  println("Stampa dei valori di verità di neg3: ")
  println(notEmpty2("foo"))
  println(notEmpty2(""))
  println(notEmpty2("foo") && !notEmpty2(""))
}
