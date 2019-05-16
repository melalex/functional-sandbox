package com.melalex.part6

import java._
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

trait Future[+A] {

  private[part6] def apply(function: A => Unit): Unit
}

object Par {

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get
  }

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => (cb: C => Unit) => {
    var ar: Option[A] = None
    var br: Option[B] = None

    val combiner = Actor[Either[A, B]](es) {
      case Left(a) =>
        if (br.isDefined) eval(es)(cb(f(a, br.get)))
        else ar = Some(a)
      case Right(b) =>
        if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
        else br = Some(b)
    }
    p(es)(a => combiner ! Left(a))
    p2(es)(b => combiner ! Right(b))
  }

  def unit[A](a: A): Par[A] =
    es => (cb: A => Unit) => cb(a)

  def delay[A](a: => A): Par[A] =
    es => (cb: A => Unit) => cb(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => (cb: A => Unit) => eval(es)(a(es)(cb))

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })
}
