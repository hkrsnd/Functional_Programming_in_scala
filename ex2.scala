package org.pii.functional_scala {
  object function {
    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
      @annotation.tailrec
      def loop(n: Int): Int = {
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)
      }

      loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      def is_ordered(n: Int): Boolean = {
        if (n + 1 >= as.length) true
        else if (ordered(as(n), as(n+1))) is_ordered(n + 1)
        else false
      }
      is_ordered(0)
    }

    def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
      (b: B) => f(a, b)

    def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
      a => b => f(a, b)
    }

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      (a, b) => f(a)(b)
    }

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      a => f(g(a))
    }
  }
}
