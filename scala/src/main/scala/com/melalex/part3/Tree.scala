package com.melalex.part3

object Tree {

  def apply[E](left: Tree[E], right: Tree[E]): Tree[E] = Branch(left, right)

  def apply[E](value: E): Tree[E] = Leaf(value)
}

sealed trait Tree[-T]

case class Leaf[T](value: T) extends Tree[T]

case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
