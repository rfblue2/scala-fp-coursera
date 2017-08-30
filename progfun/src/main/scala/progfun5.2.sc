def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(l: List[Int], r: List[Int]): List[Int] =
      (l, r) match {
        case (Nil, _) => r
        case (_, Nil) => l
        case (x :: ls, y :: rs) =>
          if (x < y) x :: merge(ls, r)
          else y :: merge(l, rs)
      }

    val (l, r) = xs splitAt n
    merge(msort(l), msort(r))
  }
}

val l1 = List(2, 72, 4, 23, 1, 37, 9)
msort(l1).toString()