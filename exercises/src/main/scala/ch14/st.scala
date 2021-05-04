package fpinscala.ch14.st

sealed trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]) = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {

  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

// Application using ST

sealed trait STRef[S, A] {

  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {

  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a // error handling?
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), z) => z.flatMap(_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(j, x)
      _ <- write(i, y)
    } yield ()

}

object STArray {

  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
}

object Main extends App {
  val p = new RunnableST[(Int, Int)] {
    def apply[S] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
  }

  println(ST.runST(p))

  println(ST.runST(new RunnableST[List[Int]] {
    def apply[S] =
      for {
        a <- STArray(10, 0)
        _ <- a.fill(Map(1 -> 1, 2 -> 2, 9 -> 9))
//        _ <- a.swap(1, 2)
        _ <- partition(a, 0, 9, 2)
        l <- a.freeze
      } yield l
  }))

  def noop[S] = ST[S, Unit](())

  def partition[S](
      arr: STArray[S, Int],
      n: Int,
      r: Int,
      pivot: Int
  ): ST[S, Int] =
    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)

      jRef <- STRef(n)
      iRef <- STRef(n)

      _ <- (n until r).foldLeft(noop[S])(
        (s, i) =>
          for {
            _ <- s
            iVal <- arr.read(i)
            _ <- if (iVal < pivotVal) for {
              j <- jRef.read
              _ <- arr.swap(i, j)
              _ <- jRef.write(j + 1)
            } yield ()
            else noop[S]
          } yield ()
      )
      j <- jRef.read
      _ <- arr.swap(j, r)
    } yield j

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
    if (n < r)
      for {
        pi <- partition(a, n, r, n + (r - n) / 2)
        _ <- qs(a, n, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S] =
          for {
            arr <- STArray.fromList(xs)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          } yield sorted
      })

  println(quicksort(List(5, 3, 6, 20, 1, 0, 0)))

}
