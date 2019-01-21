package com.melalex.part3

import com.melalex.part3.LinkedListOps._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class LinkedListSpec extends FlatSpec with MockFactory {

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

  it should "do for each" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val mockConsumer = mockFunction[Int, Unit]

    mockConsumer expects 0 once;
    mockConsumer expects 1 once;
    mockConsumer expects 2 once;
    mockConsumer expects 3 once;
    mockConsumer expects 4 once

    target.forEach(mockConsumer)
  }

  it should "map" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList("0", "1", "2", "3", "4")

    val actual = target.map(_ toString)

    assert(actual == expected)
  }

  it should "flat map" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(0, 0, 1, 1, 2, 2, 3, 3, 4, 4)

    val actual = target.flatMap(e => LinkedList(e, e))

    assert(actual == expected)
  }

  it should "filter" in {
    val target = LinkedList(0, 1, 2, 3, 4)
    val expected = LinkedList(0, 2, 4)

    val actual = target.filter(_ % 2 == 0)

    assert(actual == expected)
  }

  private def of[E](first: E, tail: LinkedList[E] = EmptyLinkedList) = NonEmptyLinkedList[E](first, tail)
}
