package fpinscala.laziness {
  import Stream._
  trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(h,t) => h() :: t().toList
      case _ => List()
    }
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    def exists(f: A => Boolean): Boolean =
      foldRight(false)((a,b) => f(a) || b)
    def forAll(f: A => Boolean): Boolean =
      foldRight(true)((a,b) => f(a) && b)
    def takeWhile2(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h,t) =>
          if (f(h)) cons(h,t)
          else empty)
    def headOption: Option[A] =
      foldRight(None: Option[A])((h,_) => Some(h))
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h,t) =>
          cons(f(h), t))
    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h,t) =>
          if(f(h)) cons(h,t)
          else t)
    def append[B>:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h,t) => cons(h,t))
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h,t) => f(h) append t)
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    val ones: Stream[Int] = Stream.cons(1, ones)
    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))
    def from(n: Int): Stream[Int] =
      cons(n, constant(n + 1))
    val fibs = {
      def rec(f0: Int, f1: Int): Stream[Int] =
        cons(f0, rec(f1, f0+f1))
      rec(0, 1)
    }
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h,s)) => cons(h, unfold(s)(f))
        case None => empty
      }
  }
}
