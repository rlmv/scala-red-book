package fpinscala.ch6.state

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(si => {
      val (a, s) = this.run(si)
      g(a).run(s)
    })

  def map[B](g: A => B): State[S, B] =
    this.flatMap(a => State.unit(g(a)))

  def map[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
