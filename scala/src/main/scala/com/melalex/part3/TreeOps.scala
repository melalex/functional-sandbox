package com.melalex.part3

import scala.annotation.tailrec

object TreeOps {

  def fold[E, R](tree: Tree[E], seed: R)(function: (R, Tree[E]) => R): R = {

    @tailrec
    def loop(trees: List[Tree[E]], acc: R): R = trees match {
      case Nil => acc
      case (leaf: Leaf[E]) :: tail => loop(tail, function(acc, leaf))
      case (branch: Branch[E]) :: tail => loop(branch.left :: branch.right :: tail, function(acc, branch))
    }

    loop(List(tree), seed)
  }

  def fold[E, R](tree: Tree[E])(function: E => R)(combine: (R, R) => R): R = tree match {
    case Leaf(value) => function(value)
    case Branch(left, right) => combine(fold(left)(function)(combine), fold(right)(function)(combine))
  }

  def size[E](tree: Tree[E]): Int = fold(tree, 0)((acc, _) => acc + 1)

  def max[E](tree: Tree[E], min: E)(comparator: (E, E) => Int): E = fold(tree, min)((max, subTree) =>
    subTree match {
      case Leaf(value) if comparator(value, max) > 0 => value
      case _ => max
    })

  def depth[E](tree: Tree[E]): Int = fold(tree)(_ => 1)((left, right) => (if (left > right) left else right) + 1)

  def map[E, R](tree: Tree[E])(transform: E => R): Tree[R] =
    fold(tree)(value => Leaf(transform(value)) : Tree[R])(Branch(_, _))

  implicit def wrapLinkedList[E](tree: Tree[E]): ExtendedTree[E] = ExtendedTree(tree)

  case class ExtendedTree[E](tree: Tree[E]) {

    def size: Int = TreeOps.size(tree)

    def depth: Int = TreeOps.depth(tree)

    @inline
    def max(min: E)(comparator: (E, E) => Int): E = TreeOps.max(tree, min)(comparator)

    @inline
    def map[R](transform: E => R): Tree[R] = TreeOps.map(tree)(transform)
  }

}