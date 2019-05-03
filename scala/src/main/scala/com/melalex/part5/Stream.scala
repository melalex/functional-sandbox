package com.melalex.part5

import com.melalex.part4.{Absent, Conditional, Existent}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def maybeHead: Conditional[A]

  @inline
  def forAll(predicate: A => Boolean): Boolean = Stream.forAll(this)(predicate)

  @inline
  def exists(predicate: A => Boolean): Boolean = Stream.exists(this)(predicate)

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

  @inline
  def zipWith[B](other: Stream[B]): Stream[(A, B)] = Stream.zip(this, other)

  @inline
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = Stream.zipAll(this, other)

  @inline
  def startsWith[B >: A](prefix: Stream[B]): Boolean = Stream.startsWith(this, prefix)

  @inline
  def tails: Stream[Stream[A]] = Stream.tails(this)

  @inline
  def scanRight[B](seed: B)(scanner: (A, B) => B): Stream[B] = Stream.scanRight(this, seed)(scanner)

  @inline
  def hasSubSequence[B >: A](subSequence: Stream[B]): Boolean = Stream.hasSubSequence(this, subSequence)
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
  def forAll[A](stream: Stream[A])(predicate: A => Boolean): Boolean = stream match {
    case Cons(head, tail) => predicate(head()) && forAll(tail())(predicate)
    case Empty => true
  }

  @tailrec
  def exists[A](stream: Stream[A])(predicate: A => Boolean): Boolean = stream match {
    case Cons(head, tail) => predicate(head()) || exists(tail())(predicate)
    case Empty => false
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
  def toList[A](stream: Stream[A]): List[A] = foldRight[A, List[A]](stream, Nil)(_ :: _)

  @tailrec
  def drop[A](stream: Stream[A], count: Int): Stream[A] = if (count <= 0) stream else stream match {
    case Empty => stream
    case Cons(_, tail) => drop(tail(), count - 1)
  }


  def take[A](stream: Stream[A], count: Int): Stream[A] = unfold((count, stream)) {
    case (remaining, Cons(head, tail)) if remaining > 0 => Some(head(), (remaining - 1, tail()))
    case _ => None
  }

  def takeWhile[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] =
    unfold(stream) {
      case Cons(head, tail) if predicate(head()) => Some(head(), tail())
      case _ => None
    }

  def map[A, B](stream: Stream[A])(mapper: A => B): Stream[B] =
    unfold(stream) {
      case Cons(head, tail) => Some(mapper(head()), tail())
      case Empty => None
    }

  def filter[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] =
    foldRight(stream, Empty: Stream[A])((a, b) => if (predicate(a)) Stream(a, b) else b)

  def flatMap[A, B](stream: Stream[A])(mapper: A => Stream[B]): Stream[B] =
    foldRight(stream, Empty: Stream[B])((a, b) => mapper(a).foldRight(b)(Stream(_, _)))

  def zip[A, B](first: Stream[A], second: Stream[B]): Stream[(A, B)] = unfold((first, second)) {
    case (Cons(firstHead, firstTail), Cons(secondHead, secondTail)) => Some((firstHead(), secondHead()), (firstTail(), secondTail()))
    case _ => None
  }

  def zipAll[A, B](first: Stream[A], second: Stream[B]): Stream[(Option[A], Option[B])] = unfold((first, second)) {
    case (Cons(firstHead, firstTail), Cons(secondHead, secondTail)) => Some((Some(firstHead()), Some(secondHead())), (firstTail(), secondTail()))
    case (Empty, Cons(secondHead, secondTail)) => Some((None, Some(secondHead())), (Empty, secondTail()))
    case (Cons(firstHead, firstTail), Empty) => Some((Some(firstHead()), None), (firstTail(), Empty))
    case _ => None
  }

  def startsWith[A](stream: Stream[A], prefix: Stream[A]): Boolean = forAll(zip(stream, prefix)) {
    case (head, prefixHead) => head == prefixHead
  }

  def tails[A](stream: Stream[A]): Stream[Stream[A]] = unfold(stream) {
    case Cons(_, tail) => Some((tail(), tail()))
    case Empty => None
  }

  def scanRight[A, B](stream: Stream[A], seed: B)(scanner: (A, B) => B): Stream[B] = foldRight(stream, Empty: Stream[B]) {
    (a, b) =>
      b match {
        case tail@Cons(head, _) => Stream(scanner(a, head()), tail)
        case Empty => Stream(scanner(a, seed))
      }
  }

  def hasSubSequence[A](stream: Stream[A], subSequence: Stream[A]): Boolean = stream.tails.exists(s => startsWith(s, subSequence))
}