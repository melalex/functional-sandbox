package com.melalex.part3

object LinkedList {

  def apply[E](es: E*): LinkedList[E] = es match {
    case Nil => EmptyLinkedList
    case head +: tail => NonEmptyLinkedList[E](head, apply(tail: _*))
  }

  def unapply[E](arg: LinkedList[E]): Option[(E, LinkedList[E])] = arg match {
    case list: NonEmptyLinkedList[E] => Some((list.head, list.tail))
    case _ => None
  }
}

sealed trait LinkedList[+E] {

  val tail: LinkedList[E]
}

object EmptyLinkedList extends LinkedList[Nothing] {

  val tail: LinkedList[Nothing] = this
}

case class NonEmptyLinkedList[E](head: E, tail: LinkedList[E]) extends LinkedList[E] {

  override def toString: String = "[ " + LinkedListOps.foldLeft(this, new StringBuilder())((result, node) => result.append(node).append(", ")) + "]"
}