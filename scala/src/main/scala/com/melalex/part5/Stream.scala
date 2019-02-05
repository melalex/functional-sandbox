package com.melalex.part5

import com.melalex.part4.{Absent, Conditional, Existent}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def maybeHead: Conditional[A]

  @inline
  def foldLeft[B](seed: B)(folder: (A, => B) => B): B = Stream.foldLeft(this, seed)(folder)

  @inline
  def foldRight[B](seed: B)(folder: (A, B) => B): B = Stream.foldRight(this, seed)(folder)

  @inline
  def toList: List[A] = Stream.toList(this)

  @inline
  def drop(count: Int): Stream[A] = Stream.drop(this, count)

  @inline
  def take(count: Int): Stream[A] = Stream.take(this, count)

  @inline
  def takeWhile(predicate: A => Boolean): Stream[A] = Stream.takeWhile(this)(predicate)

  @inline
  def map[B](mapper: A => B): Stream[B] = Stream.map(this)(mapper)

  @inline
  def filter(predicate: A => Boolean): Stream[A] = Stream.filter(this)(predicate)

  @inline
  def flatMap[B](mapper: A => Stream[B]): Stream[B] = Stream.flatMap(this)(mapper)
}

case object Empty extends Stream[Nothing] {

  override def maybeHead: Conditional[Nothing] = Absent
}

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {

  override def maybeHead: Conditional[A] = Existent(head())
}

object Stream {

  def apply[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val lazyHead = head
    lazy val lazyTail = tail

    Cons(() => lazyHead, () => lazyTail)
  }

  def apply[A](): Stream[A] = Empty

  @inline
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else apply(as.head, apply(as.tail: _*))

  def unfold[V, S](initial: S)(supplier: S => Option[(V, S)]): Stream[V] = supplier(initial) match {
    case Some((elem, next)) => Stream(elem, unfold(next)(supplier))
    case None => Empty
  }

  @tailrec
  def foldLeft[A, B](stream: Stream[A], seed: B)(folder: (A, => B) => B): B = stream match {
    case Empty => seed
    case Cons(head, tail) => foldLeft(tail(), folder(head(), seed))(folder)
  }

  @inline
  def foldRight[A, B](stream: Stream[A], seed: B)(folder: (A, B) => B): B =
    foldLeft(stream, (r: B) => r)((node, result) => r => result(folder(node, r)))(seed)

  @inline
  def toList[A](stream: Stream[A]): List[A] = Stream.foldRight[A, List[A]](stream, Nil)(_ :: _)

  @tailrec
  def drop[A](stream: Stream[A], count: Int): Stream[A] = if (count == 0) stream else stream match {
    case Empty => stream
    case Cons(_, tail) => drop(tail(), count - 1)
  }

  def take[A](stream: Stream[A], count: Int): Stream[A] = if (count == 0) Empty else stream match {
    case Cons(head, tail) => Stream(head(), take(tail(), count - 1))
    case Empty => Empty
  }

  def takeWhile[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] =
    foldRight(stream, Empty: Stream[A])((a, b) => if (predicate(a)) Stream(a, b) else Stream())

  def map[A, B](stream: Stream[A])(mapper: A => B): Stream[B] =
    foldRight(stream, Empty: Stream[B])((a, b) => Stream(mapper(a), b))

  def filter[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] =
    foldRight(stream, Empty: Stream[A])((a, b) => if (predicate(a)) Stream(a, b) else b)

  def flatMap[A, B](stream: Stream[A])(mapper: A => Stream[B]): Stream[B] =
    foldRight(stream, Empty: Stream[B])((a, b) => mapper(a).foldRight(b)(Stream.apply(_, _)))
}