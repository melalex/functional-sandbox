package com.melalex.part3

abstract class LinkedList<T> : Iterable<T> {

    abstract fun append(element: T): NonEmptyLinkedList<T>

    abstract fun <R> reduce(identity: R, accumulator: (R, T) -> R): R

    companion object {

        @Suppress("UNCHECKED_CAST")
        fun <T> of(): LinkedList<T> = EmptyLinkedList as LinkedList<T>

        fun <T> of(first: T, vararg elements: T): NonEmptyLinkedList<T> {
            tailrec fun createList(source: Array<out T>, index: Int, result: LinkedList<T>): LinkedList<T> {
                return if (1 > index) result
                else createList(source, index - 1, result.append(source[index]))
            }

            @Suppress("UNCHECKED_CAST")
            return createList(elements, elements.size - 1, EmptyLinkedList as LinkedList<T>).append(first)
        }
    }
}

object EmptyLinkedList : LinkedList<Any>() {

    override fun append(element: Any): NonEmptyLinkedList<Any> = NonEmptyLinkedList(element, this)

    override fun <R> reduce(identity: R, accumulator: (result: R, element: Any) -> R): R = identity

    override fun iterator(): Iterator<Any> = object : Iterator<Any> {

        override fun hasNext(): Boolean = false

        override fun next(): Nothing = throw NoSuchElementException("LinkedList is empty")
    }
}

class NonEmptyLinkedList<T>(val head: T, val tail: LinkedList<T>) : LinkedList<T>() {

    override fun append(element: T): NonEmptyLinkedList<T> = NonEmptyLinkedList(element, this)

    override fun <R> reduce(identity: R, accumulator: (R, T) -> R): R {
        tailrec fun reduceInternal(list: LinkedList<T>, result: R): R =
                if (list is NonEmptyLinkedList<T>) reduceInternal(list.tail, accumulator.invoke(result, head))
                else result

        return reduceInternal(this, identity)
    }

    override fun iterator(): Iterator<T> = object : Iterator<T> {

        private var target: LinkedList<T> = this@NonEmptyLinkedList

        override fun hasNext(): Boolean = target is NonEmptyLinkedList<T>

        override fun next(): T {
            val list = target

            if (list is NonEmptyLinkedList<T>) {
                val next = list.head

                target = list.tail

                return next
            } else {
                throw NoSuchElementException("No more elements")
            }
        }
    }
}