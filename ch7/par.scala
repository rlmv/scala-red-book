import java.util.concurrent.{ExecutorService, Executors, Future, Callable}
import scala.concurrent.duration.TimeUnit

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get()))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def delay[A](a: => Par[A]): Par[A] =
    es => a(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((a, accum) => map2(a, accum)(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val pbs: List[Par[B]] = as.map(asyncF(f))
    sequence(pbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas = parMap(as)(a => (a, f(a)))
    map(pas)(_.filter(_._2).map(_._1))
  }

  def parFold[A](as: IndexedSeq[A])(base: A)(f: (A, A) => A): Par[A] = {
    if (as.size <= 1)
      unit(as.headOption match {
        case Some(a) => f(base, a)
        case None    => base
      })
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(parFold(l)(base)(f), parFold(r)(base)(f))(f)
    }
  }
  // def parTraverse[A](as: List[A])(f
  // def countWords(paragraphs: List[String]): Par[Int] = {
  //  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))
//    es => f(a(es).get)(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(x => choices(x))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val b = a(es).get
      b(es)
    }

  def join2[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)
}

object Main extends App {

  import Par._

  val executor: ExecutorService = Executors.newFixedThreadPool(10)

  def sumV1(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sumV1(l), sumV1(r))(_ + _)
    }
  }

  def sumV2(ints: IndexedSeq[Int]): Par[Int] = parFold(ints)(0)(_ + _)

  // TODO: blocked because f must be (A, A) => A for parFold
  // def max(ints: IndexedSeq[Int]): Par[Option[Int]] = parFold(ints)(None)(
  //   (accum, i) => accum.map(math.max(_, i)).orElse(Some(i)))

  def countWords(paragraphs: List[String]): Par[Int] = {
    val pCounts = parMap(paragraphs)(p => p.split("\\s+").length)
    map(pCounts)(_.sum)
  }

  def getR[A](p: Par[A]): Unit = println(run(executor)(p).get())

  getR(parFilter(List(1, 2, 4, 5))(_ % 2 == 0))

  getR(sumV1(IndexedSeq(1, 2, 3, 4, 5)))
  getR(sumV2(IndexedSeq(1, 2, 3, 4, 5)))

  getR(countWords(List("his hsf", " bbb bb", "5\n6")))

  executor.shutdown()
}
