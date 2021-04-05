sealed trait MySeq[+A]
object MySeq {
  case object MyNil extends MySeq[Nothing]
  final case class Zero[A](seq: MySeq[(A, A)]) extends MySeq[A]
  final case class One[A](e: A, seq: MySeq[(A, A)]) extends MySeq[A]

  def unit[A](a: A): MySeq[A] = One(a, MyNil)

  def cons[A](a: A, seq: MySeq[A]): MySeq[A] =
    seq match {
      case MyNil       => One(a, MyNil)
      case z @ Zero(s) => One(a, s)
      case One(x, s)   => Zero(cons((a, x), s))
    }

  def uncons[A](seq: MySeq[A]): (Option[A], MySeq[A]) =
    seq match {
      case MyNil => (None, MyNil)
      case z @ Zero(s) =>
        uncons(s) match {
          case (Some((x, y)), seq_) => (Some(x), One(y, seq_))
          case (None, _)            => (None, z)
        }
      case One(x, rest) => (Some(x), Zero(rest))
    }
  def headOption[A](seq: MySeq[A]): Option[A] = {
    val (h, _) = uncons(seq)
    h
  }
  def tail[A](seq: MySeq[A]): MySeq[A] = {
    val (_, tail) = uncons(seq)
    tail
  }

  def lookup[A](i: Int, seq: MySeq[A]): Option[A] =
    (i, seq) match {
      case (_, MyNil)     => None
      case (0, One(x, _)) => Some(x)
      case (i, One(x, s)) => lookup(i - 1, Zero(s))
      case (i, Zero(s)) =>
        lookup(i / 2, s) match {
          case None                       => None
          case Some((x, _)) if i % 2 == 0 => Some(x)
          case Some((_, y))               => Some(y)
        }
    }
}

import MySeq._

println(lookup(2, cons(1, cons(2, cons(3, cons(4, cons(5, unit(0))))))))
