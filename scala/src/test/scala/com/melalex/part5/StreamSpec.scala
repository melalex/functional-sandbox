package com.melalex.part5

import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {

  "Stream" should "return Absent head" in {
    val target = Stream[Int]()

    val actual = target.maybeHead

    assert(!actual.present)
  }

  it should "return Existent head" in {
    val target = Stream(1)

    val actual = target.maybeHead

    assert(actual.present)
  }

  it should "foldLeft" in {
    val target = Stream(1, 2, 3, 4)

    val actual = target.foldLeft(0)(_ + _)

    assert(actual == 10)
  }

  it should "foldRight" in {
    val target = Stream(1, 2, 3, 4)

    val actual = target.foldRight(0)(_ + _)

    assert(actual == 10)
  }

  it should "convert to List" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 1 :: 2 :: 3 :: 4 :: Nil

    val actual = target.toList

    assert(actual == expected)
  }

  it should "drop" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 3 :: 4 :: Nil

    val actual = target.drop(2)

    assert(actual.toList == expected)
  }

  it should "take" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 1 :: 2 :: Nil

    val actual = target.take(2)

    assert(actual.toList == expected)
  }

  it should "takeWhile" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 1 :: 2 :: Nil

    val actual = target.takeWhile(_ != 3)

    assert(actual.toList == expected)
  }

  it should "map" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 2 :: 4 :: 6 :: 8 :: Nil

    val actual = target.map(_ * 2).toList

    assert(actual == expected)
  }

  it should "flatMap" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: 4 :: 4 :: Nil

    val actual = target.flatMap(a => Stream(a, a)).toList

    assert(actual == expected)
  }

  it should "filter" in {
    val target = Stream(1, 2, 3, 4)
    val expected = 2 :: 4 :: Nil

    val actual = target.filter(_ % 2 == 0).toList

    assert(actual == expected)
  }
}
