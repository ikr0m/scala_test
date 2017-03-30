import scala.annotation.tailrec

// factorial tail rec
def fact(n: Int): Long = {
  @tailrec
  def go(m: Int, acc: Long): Long = {
    if (m == 1) {
      acc
    } else {
      go(m - 1, acc * m)
    }
  }
  go(n, 1)
}

fact(1)
fact(2)
fact(3)
fact(4)

// ---------------
{
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def multiply(a: Int, b: Int) = a * b

  def multiplyCurry = curry(multiply)

  def multiply3To = multiplyCurry(3)
  def multiply5To = multiplyCurry(5)

  val r1 = multiply3To(5)
  val r2 = multiply5To(6)
  (r1, r2)
}

// ---------------
{
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def multiplyToStrLength = (i: Int) => (str: String) => i * str.length

  //  def multiply(a: Int)(b: Int) = a * b

  def multiplyUncurry = uncurry(multiplyToStrLength)

  val r4 = multiplyUncurry(3, "test")
  val r5 = multiplyUncurry(4, "hi")
  (r4, r5)
}

// ---------------
{
  def partialMultiply[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  def multiply(a: Int, b: Int) = a * b

  def multiplyCurry = partialMultiply(3, multiply)

  val r1 = multiplyCurry(3)
  val r2 = multiplyCurry(5)
  (r1, r2)
}
