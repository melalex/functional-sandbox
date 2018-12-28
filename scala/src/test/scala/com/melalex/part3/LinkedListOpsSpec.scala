package com.melalex.part3

import com.melalex.part3.LinkedListOps._
import org.scalatest.FlatSpec

class LinkedListOpsSpec extends FlatSpec {

  "The LinkedList" should "be flatten" in {
    val target = LinkedList(
      LinkedList(0, LinkedList(1, 2)),
      LinkedList(3),
      LinkedList(4),
    )

    val actual = flatten(target)(i => i.asInstanceOf[Int])

    val expected = LinkedList(0, 1, 2, 3, 4)

    assert(actual == expected)
  }
}
