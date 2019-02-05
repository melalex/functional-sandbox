package com.melalex.part4

object Util {

  def mean(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some(seq.sum / seq.length)

  def variance(seq: Seq[Double]): Option[Double] = mean(seq).flatMap { m => mean(seq.map(x => math.pow(x - m, 2))) }
}
