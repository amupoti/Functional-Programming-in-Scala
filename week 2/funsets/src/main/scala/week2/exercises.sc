object exercise {

  def product(f: Int => Int)(a: Int, b: Int): Int = {

    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  product(x => x * x * x)(1, 3)

  def factorial(a: Int): Int = {
    product(x => x)(1, a)
  }

  factorial(4)

  def generic(f: Int => Int, identity: Int, arith: (Int, Int) => Int)(a: Int, b: Int): Int = {

    if (a > b) identity
    else arith(f(a), generic(f, identity, arith)(a + 1, b))

  }

  generic(x => x, 1, (z, y) => z * y)(1, 5)
}
