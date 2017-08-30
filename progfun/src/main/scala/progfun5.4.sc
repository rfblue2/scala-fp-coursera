object session {
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, last) = xs span (y => y == x)
      first :: pack(last)
    }
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(x => (x.head, x.length))

  val data = List("a", "a", "a", "b", "c", "c", "a")
  pack(data)
  encode(data)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (_, l) => l + 1 )

  mapFun(List(1, 2, 3, 10), (x: Int) => x*x)
  lengthFun(List(1, 2, 3, 10))
}