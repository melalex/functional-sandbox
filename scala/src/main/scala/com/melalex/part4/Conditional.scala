package com.melalex.part4

import scala.annotation.tailrec
import scala.collection.immutable.Nil

object Conditional {

  def apply[A](): Conditional[A] = Absent

  def apply[A](value: A): Conditional[A] = Existent(value)

  def map2[A, B, C](a: Conditional[A], b: Conditional[B])(f: (A, B) => C): Conditional[C] = (a, b) match {
    case (Existent(aValue), Existent(bValue)) => Existent(f(aValue, bValue))
    case _ => Conditional()
  }

  def sequence[A](list: List[Conditional[A]]): Conditional[List[A]] = traverse(list)(identity)

  def traverse[A, B](list: List[A])(f: A => Conditional[B]): Conditional[List[B]] = {

    @tailrec
    def loop(remaining: List[A], result: List[B]): Conditional[List[B]] = remaining match {
      case head :: tail => f(head) match {
        case Existent(value) => loop(tail, result :+ value)
        case Absent => Conditional()
      }
      case Nil => Conditional(result)
    }

    loop(list, Nil)
  }
}

sealed trait Conditional[+A] {

  val present: Boolean

  def map[B](f: A => B): Conditional[B]

  def flatMap[B](f: A => Conditional[B]): Conditional[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Conditional[B]): Conditional[B]

  def filter(f: A => Boolean): Conditional[A]
}

case class Existent[+A](value: A) extends Conditional[A] {

  val present = true

  override def map[B](f: A => B): Conditional[B] = Existent(f(value))

  override def flatMap[B](f: A => Conditional[B]): Conditional[B] = f(value)

  override def getOrElse[B >: A](default: => B): B = value

  override def orElse[B >: A](ob: => Conditional[B]): Conditional[B] = this

  override def filter(f: A => Boolean): Conditional[A] = if (f(value)) this else Absent
}

object Absent extends Conditional[Nothing] {

  val present = false

  override def map[B](f: Nothing => B): Conditional[B] = this

  override def flatMap[B](f: Nothing => Conditional[B]): Conditional[B] = this

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Conditional[B]): Conditional[B] = ob

  override def filter(f: Nothing => Boolean): Conditional[Nothing] = this
}
