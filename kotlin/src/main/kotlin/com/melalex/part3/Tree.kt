package com.melalex.part3

interface Tree<out A>

data class Branch<out A>(val left: Tree<A>, val right: Tree<A>) : Tree<A>

data class Leaf<out A>(val value: A) : Tree<A>

fun <A, B> Tree<A>.fold(function: (A) -> B, combine: (B, B) -> B): B = when (this) {
    is Leaf<A> -> function(value)
    is Branch<A> -> combine(left.fold(function, combine), right.fold(function, combine))
    else -> throw IllegalStateException("Unknown [ com.melalex.part3.Tree ] subclass")
}

fun <A> Tree<A>.size(): Int = fold({ 1 }) { left, right -> left + right + 1 }

fun <A> Tree<A>.depth(): Int = fold({ 1 }) { left, right -> (if (left > right) left else right) + 1 }

fun <A> Tree<A>.max(comparator: Comparator<A>): A = fold({ it }) { left, right ->
    if (comparator.compare(left, right) > 0) left else right
}

fun <A, B> Tree<A>.map(transformer: (A) -> B): Tree<B> = fold({ leaf(transformer(it)) }) { left, right -> Branch(left, right) }

private fun <A> leaf(value: A): Tree<A> = Leaf(value)