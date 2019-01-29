package com.melalex.part3

import com.melalex.part3.TreeOps._
import org.scalatest.FlatSpec

import scala.math.Ordering

class TreeSpec extends FlatSpec {

  "The tree" should "be created" in {
    val actual = Tree(Tree(10), Tree(10))

    assert(actual == actual)
  }

  it should "provide size" in {
    val actual = Tree(Tree(10), Tree(10)).size

    assert(actual == 3)
  }

  it should "provide depth" in {
    val actual = Tree(Tree(Tree(10), Tree(10)), Tree(10)).depth

    assert(actual == 3)
  }

  it should "provide max" in {
    val target = Tree(Tree(Tree(10), Tree(20)), Tree(Tree(40), Tree(30)))

    val actual = target.max(Int.MinValue)(Ordering.Int.compare)

    assert(actual == 40)
  }

  it should "map" in {
    val target = Tree(Tree(Tree(10), Tree(20)), Tree(Tree(40), Tree(30)))
    val expected = Tree(Tree(Tree("10"), Tree("20")), Tree(Tree("40"), Tree("30")))

    val actual = target.map(_ toString)

    assert(actual == expected)
  }
}