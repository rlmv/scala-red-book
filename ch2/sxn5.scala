object Session extends App {

  def findFirst[A](ss: List[A], key: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= ss.length)
        -1
      else if (key(ss(n)))
        n
      else
        loop(n + 1)
    }
    loop(0)
  }

  println(findFirst(List("do", "re", "mi"), (x: String) => x == "re"))
  println(findFirst(List(0, 1, 2, 3), (x: Int) => x == 5))
}
