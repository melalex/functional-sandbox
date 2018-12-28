package com.melalex.part3

import com.melalex.part3.LinkedListOps._
import org.scalatest.FlatSpec

class LinkedListSpec extends FlatSpec {

  "The LinkedList" should "be created" in {
    val actual = LinkedList(0, 1, 2, 3, 4)

    val expected = of(0, of(1, of(2, of(3, of(4)))))

    assert(actual == expected)
  }

  it should "fold right" in {
    val target = LinkedList(0, 1, 2, 3, 4)

    assert(10 == target.foldRight[Int](0, _ + _))
  }

  it should "fold left" in {
    val target = LinkedList(0, 1, 2, 3, 4)

    assert(10 == target.foldLeft[Int](0, _ + _))
  }

  it should "set head" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(20, 1, 2, 3, 4)

    val actual = target.setHead(20)

    assert(actual == expected)
  }

  it should "drop" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(2, 3, 4)

    val actual = target.drop(2)

    assert(actual == expected)
  }

  it should "drop while" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(2, 3, 4)

    val actual = target.dropWhile(e => e < 2)

    assert(actual == expected)
  }

  it should "drop right" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(0, 1, 2)

    val actual = target.dropRight(2)

    assert(actual == expected)
  }

  private def of[E](first: E, tail: LinkedList[E] = EmptyLinkedList) = new NonEmptyLinkedList[E](first, tail)
}
