package com.melalex.part5

interface Stream<out A> {

    val headOption: A?
}

object Empty : Stream<Any> {

    override val headOption = null
}

data class Cons<A>(val head: () -> A, val tail: () -> Stream<A>) : Stream<A> {

    override val headOption: A
        get() = head()
}

fun <A, B> Stream<A>.foldLeft(seed: B, folder: (A, B) -> B): B = when (this) {
    is Empty -> seed
    is Cons<A> -> foldLeft(folder(head(), seed), folder)
    else -> throw IllegalStateException("Unknown [ com.melalex.part5.Stream ] subclass")

}

fun <A, B> Stream<A>.foldRight(seed: B, folder: (A, B) -> B): B =
        foldLeft<A, (B) -> B>({ it }) { a, b -> { r -> b(folder(a, r)) } }(seed)

fun <A> Stream<A>.toList(): List<A> = foldRight(mutableListOf()) { a, list -> list.add(a); list }

fun <A> Stream<A>.drop(count: Int): Stream<A> = when {
    count == 0 -> this
    this is Empty -> this
    this is Cons<A> -> tail().drop(count - 1)
    else -> throw IllegalStateException("Unknown [ com.melalex.part5.Stream ] subclass")
}

@Suppress("UNCHECKED_CAST")
fun <A> Stream<A>.take(count: Int): Stream<A> = when {
    this is Cons<A> && count > 0 -> Cons(head, { take(count - 1) })
    else -> Empty as Stream<A>
}

@Suppress("UNCHECKED_CAST")
fun <A> Stream<A>.takeWhile(predicate: (A) -> Boolean): Stream<A> = when {
    this is Cons<A> && predicate(head()) -> Cons(head, { takeWhile(predicate) })
    else -> Empty as Stream<A>
}

@Suppress("UNCHECKED_CAST")
fun <A, B> unfold(seed: B, supplier: (B) -> Pair<A, B>?): Stream<A> =
        supplier(seed)?.let { Cons({ it.first }, { unfold(it.second, supplier) }) } ?: Empty as Stream<A>