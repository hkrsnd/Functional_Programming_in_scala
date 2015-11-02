package org.pii.function_scala {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def tail[A](a: List[A]): List[A] = {
      a match {
        case Nil => sys.error("tail of empty list")
        case Cons(_,t) => t
      }
    }

    def setHead[A](a: List[A], s: A): List[A] = {
      a match {
        case Nil => sys.error("setHead of empty list")
        case Cons(_,t) => Cons(s,t)
      }
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_,ls) => drop(ls, n - 1)
      }
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def myappend[A](l: List[A], r: List[A]): List[A] =
      foldRight(l,r)(Cons(_,_))

    def append[A](a: List[A], b: List[A]): List[A] = {
      a match {
        case Nil => b
        case Cons(s,t) => Cons(s,append(t,b))
      }
    }

    def concat[A](as: List[List[A]]): List[A] = {
      foldRight(as, Nil:List[A])(append)
    }

    def mapAddOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h,t) => Cons(h+1,t))

    def doubleToStinrg(l: List[Double]): List[String] =
      foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil:List[B])((h,t) => Cons(f(h),t))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
      concat(map(as)(f))
    }

    def eachAdd(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (Cons(x,xs),Cons(y,ys)) => Cons(x+y, eachAdd(xs,ys))
    }

    def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }
}
