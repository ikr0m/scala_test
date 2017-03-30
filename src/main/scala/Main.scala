object Main extends App {
  println("Started...")

  private def compose[A, B, C](f: A => B, g: B => C): A => C = {
    a => g(f(a))
  }

  println("Test1\n-----------------")
  val names = List("John Smith", "Mike Adams", "Al Adam")

  private def strToArray(str: String): Array[String] = {
    str.split("[\\W]+")
  }

  private def countLowerCasedWords(strs: Array[String]): Int = {
    strs.count(s => s.toLowerCase == s)
  }

  val composed: (String) => Int = compose(strToArray, countLowerCasedWords)
  val count = composed("Some test Test jump")
  println(s"Lowercased words count: $count")

  println("--------------\nTest2\n------------------")

  private def findIndex(elms: List[String], pred: String => Boolean): Int = {
    def go(elements: List[String], index: Int): Int = {
      elements match {
        case head :: tail =>
          if (pred(head)) {
            index
          } else {
            go(tail, index + 1)
          }
        case _ => -1
      }
    }

    go(elms, 0)
  }

  def startsWithUppercase(s: String) = s.head.isUpper
  val ind = findIndex(List("ab", "bcd", "Hello", "1", "John"), startsWithUppercase)
  println(s"index: $ind")

  // ------------------

  private def fact(n: Int): Long = {
    if (n == 1) 1 else n * fact(n - 1)
  }

  println(s"factorial(20)=${fact(20)}")

}