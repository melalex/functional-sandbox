package com.melalex.part6

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = ex => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ex => UnitFuture(f(a(ex).get(), b(ex).get()))

  def fork[A](par: => Par[A]): Par[A] = ex => ex.submit(() => par(ex).get())

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(par: Par[A]): Future[A] = par(ex)

  def async[A, B](f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  private case class UnitFuture[A](private val value: A) extends Future[A] {

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = value

    override def get(timeout: Long, unit: TimeUnit): A = value
  }

}
