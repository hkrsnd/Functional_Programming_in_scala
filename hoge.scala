
package org.pii.fpinscala {

  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      val y: Int = throw new Exception("fail!")
      try {
        val x = 42 + 5
        x + y
      }
      catch { case e: Exception => 43 }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty)
        None
      else Some(xs.sum / xs.length)
  }
}
