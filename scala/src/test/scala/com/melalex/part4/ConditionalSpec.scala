package com.melalex.part4

import org.scalatest.FlatSpec

class ConditionalSpec extends FlatSpec {

  "Two `Existent` `Conditional`s" should "map2 to `Existent`" in {
    val actual = Conditional.map2(Conditional(2), Conditional(2))(_ + _)

    assert(actual == Conditional(4))
  }

  "Two `Empty` `Conditional`s" should "map2 to `Empty`" in {
    val actual = Conditional.map2[Int, Int, Int](Conditional(), Conditional())(_ + _)

    assert(actual == Conditional())
  }

  "One `Empty` and one `Existent` `Conditional`" should "map2 to `Empty`" in {
    val actual = Conditional.map2[Int, Int, Int](Conditional(2), Conditional())(_ + _)

    assert(actual == Conditional())
  }

  "List of `Existent` `Conditional`s" should "be sequenced to `Existent`" in {
    val target = List(Conditional(0), Conditional(1), Conditional(2))

    val actual = Conditional.sequence(target)

    assert(actual == Conditional(List(0, 1, 2)))
  }

  "List of `Conditional`s with `Empty`" should "be sequenced to `Empty`" in {
    val target = List(Conditional(0), Absent, Conditional(2))

    val actual = Conditional.sequence(target)

    assert(actual == Absent)

  }

  it should "be sequenced to `Empty` when Empty on last position" in {
    val target = List(Conditional(0), Conditional(1), Absent)

    val actual = Conditional.sequence(target)

    assert(actual == Absent)
  }
}
