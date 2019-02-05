package com.melalex.part4

interface Option<A> {

    fun <B> map(f: (A) -> B): Option<B>

    fun <B> flatMap(f: (A) -> Option<B>): Option<B>

    fun getOrElse(default: () -> A): A

    fun orElse(ob: () -> Option<A>): Option<A>

    fun filter(f: (A) -> Boolean): Option<A>
}

@Suppress("UNCHECKED_CAST")
object None : Option<Any> {

    override fun <B> map(f: (Any) -> B): Option<B> = this as Option<B>

    override fun <B> flatMap(f: (Any) -> Option<B>): Option<B> = this as Option<B>

    override fun getOrElse(default: () -> Any): Any = default.invoke()

    override fun orElse(ob: () -> Option<Any>): Option<Any> = ob.invoke()

    override fun filter(f: (Any) -> Boolean): Option<Any> = this
}

@Suppress("UNCHECKED_CAST")
data class Some<A>(val value: A) : Option<A> {

    override fun <B> map(f: (A) -> B): Option<B> = Some(f(value))

    override fun <B> flatMap(f: (A) -> Option<B>): Option<B> = f(value)

    override fun getOrElse(default: () -> A): A = value

    override fun orElse(ob: () -> Option<A>): Option<A> = this

    override fun filter(f: (A) -> Boolean): Option<A> = if (f(value)) this else None as Option<A>
}

@Suppress("UNCHECKED_CAST")
fun <A, B, C> map2(a: Option<A>, b: Option<B>, f: (A, B) -> C): Option<C> = when {
    a is Some<A> && b is Some<B> -> Some(f(a.value, b.value))
    else -> None as Option<C>
}

fun <A> sequence(list: List<Option<A>>): Option<List<A>> {
    val result = mutableListOf<A>()

    for (option in list) {
        @Suppress("UNCHECKED_CAST")
        when (option) {
            is Some<A> -> result.add(option.value)
            is None -> return None as Option<List<A>>
        }
    }

    return Some(result)
}