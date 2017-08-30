object session {
  def product(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc else loop(a + 1, acc * a)
    loop(a, 1)
  }

  def product2(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product2(f)(a+1, b)
  }

  def factorial(x: Int): Int =
    product2(x => x)(1, x)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }
  product(2, 7)
  product2(x => x*x)(3, 7)
  factorial(6)
  mapReduce(x=> x*x, (a, b) => a*b, 1)(3, 7)
}