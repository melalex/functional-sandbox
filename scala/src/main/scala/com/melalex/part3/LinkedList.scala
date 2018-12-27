package com.melalex.part3

import scala.annotation.tailrec

object LinkedList {

  def apply[E](): LinkedList[E] = EmptyLinkedList

  def apply[E](head: E, tail: E*): LinkedList[E] = {

    @tailrec
    def createFromSeq(seq: Seq[E], result: LinkedList[E]): LinkedList[E] =
      if (seq.isEmpty) result
      else createFromSeq(seq.dropRight(1), result.append(seq.last))

    createFromSeq(tail, EmptyLinkedList).append(head)
  }
}

sealed trait LinkedList[+E] {

  def append[R >: E](element: R): LinkedList[R]

  def reduce[R](seed: R, accumulator: (R, E) => R): R
}

object EmptyLinkedList extends LinkedList[Nothing] {

  override def append[R >: Nothing](element: R): LinkedList[R] = new NonEmptyLinkedList[R](element, this)

  override def reduce[R](seed: R, accumulator: (R, Nothing) => R): R = seed
}

case class NonEmptyLinkedList[E](head: E, tail: LinkedList[E]) extends LinkedList[E] {

  override def append[R >: E](element: R): LinkedList[R] = new NonEmptyLinkedList[R](element, this)

  override def reduce[R](seed: R, accumulator: (R, E) => R): R =
    tail.reduce(accumulator.apply(seed, head), accumulator)
}