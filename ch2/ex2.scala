object Exercise extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else ordered(as(n), as(n + 1)) && loop(n + 1)
    }
    loop(0)
  }

  assert(isSorted(Array(0, 1, 2), (x: Int, y: Int) => x < y))
//  assert(!isSorted(Array("
}
