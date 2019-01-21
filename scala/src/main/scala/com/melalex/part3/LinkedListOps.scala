package com.melalex.part3

import scala.annotation.tailrec

object LinkedListOps {

  @inline
  def append[E, R >: E](target: LinkedList[E], node: R): NonEmptyLinkedList[R] = NonEmptyLinkedList(node, target)

  @tailrec
  def append[E, R >: E](target: LinkedList[E], other: LinkedList[R]): LinkedList[R] = target match {
    case EmptyLinkedList => other
    case NonEmptyLinkedList(head, tail) => append(tail, NonEmptyLinkedList(head, other))
  }

  def length[E](target: LinkedList[E]): Int = foldLeft(target, 0)((r, _) => r + 1)

  def foldRight[E, R](target: LinkedList[E], seed: R)(accumulator: (R, E) => R): R =
    foldLeft(target, (r: R) => r)((result, node) => r => result(accumulator(r, node)))(seed)

  @tailrec
  def foldLeft[E, R](target: LinkedList[E], seed: R)(accumulator: (R, E) => R): R = target match {
    case EmptyLinkedList => seed
    case NonEmptyLinkedList(head, tail) => foldLeft(tail, accumulator.apply(seed, head))(accumulator)
  }

  @inline
  def setHead[E, R >: E](target: LinkedList[E], node: R): NonEmptyLinkedList[R] = target match {
    case EmptyLinkedList => NonEmptyLinkedList(node, EmptyLinkedList)
    case NonEmptyLinkedList(_, tail) => NonEmptyLinkedList(node, tail)
  }

  @tailrec
  def drop[E](target: LinkedList[E], n: Int): LinkedList[E] = target match {
    case _ if n == 0 => target
    case EmptyLinkedList => EmptyLinkedList
    case NonEmptyLinkedList(_, tail) => drop(tail, n - 1)
  }

  @tailrec
  def dropWhile[E](target: LinkedList[E])(predicate: E => Boolean): LinkedList[E] = target match {
    case EmptyLinkedList => EmptyLinkedList
    case NonEmptyLinkedList(head, tail) if predicate.apply(head) => dropWhile(tail)(predicate)
    case _ => target
  }

  def dropRight[E](target: LinkedList[E], n: Int): LinkedList[E] = {
    def dropRightInternal(sliceToDrop: LinkedList[E], nodeToAdd: LinkedList[E]): LinkedList[E] =
      sliceToDrop match {
        case EmptyLinkedList => EmptyLinkedList
        case NonEmptyLinkedList(_, nextSlice) =>
          val castedNode = nodeToAdd.asInstanceOf[NonEmptyLinkedList[E]]
          dropRightInternal(nextSlice, castedNode.tail.asInstanceOf[NonEmptyLinkedList[E]]).append(castedNode.head)
      }

    val toDrop = drop(target, n)

    dropRightInternal(toDrop, target)
  }

  def reverse[E](target: LinkedList[E]): LinkedList[E] =
    foldRight(target, EmptyLinkedList.asInstanceOf[LinkedList[E]])(_ append _)

  def flatten[E](source: LinkedList[Any], target: LinkedList[E] = EmptyLinkedList): LinkedList[E] =
    foldRight[Any, LinkedList[E]](source, target)((result: LinkedList[E], node: Any) => node match {
      case list: NonEmptyLinkedList[E] => flatten(list, result)
      case EmptyLinkedList => result
      case _: E => append(result, node.asInstanceOf[E])
    })

  implicit def wrapLinkedList[E](linkedList: LinkedList[E]): LinkedListExt[E] = LinkedListExt(linkedList)

  case class LinkedListExt[+E](linkedList: LinkedList[E]) {

    @inline
    def append[R >: E](node: R): NonEmptyLinkedList[R] = LinkedListOps.append(linkedList, node)

    @inline
    def append[R >: E](other: LinkedList[R]): LinkedList[R] = LinkedListOps.append(linkedList, other)

    @inline
    def length(): Int = LinkedListOps.length(linkedList)

    @inline
    def foldRight[R](seed: R, accumulator: (R, E) => R): R = LinkedListOps.foldRight(linkedList, seed)(accumulator)

    @inline
    def foldLeft[R](seed: R, accumulator: (R, E) => R): R = LinkedListOps.foldLeft(linkedList, seed)(accumulator)

    @inline
    def setHead[R >: E](node: R): NonEmptyLinkedList[R] = LinkedListOps.setHead(linkedList, node)

    @inline
    def drop(n: Int): LinkedList[E] = LinkedListOps.drop(linkedList, n)

    @inline
    def dropWhile(predicate: E => Boolean): LinkedList[E] = LinkedListOps.dropWhile(linkedList)(predicate)

    @inline
    def dropRight(n: Int): LinkedList[E] = LinkedListOps.dropRight(linkedList, n)

    @inline
    def reverse(): LinkedList[E] = LinkedListOps.reverse(linkedList)
  }

}
