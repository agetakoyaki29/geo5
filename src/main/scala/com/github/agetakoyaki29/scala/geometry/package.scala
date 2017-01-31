package com.github.agetakoyaki29.scala

package object geometry {
  implicit def d2mrd(d: Double) = new MyRichDouble(d)

  def tupled[T1, T2, R](func: Function2[T1, T2, R]) = Function.tupled(func) // equal (import Function.tupeld)
  // implicit class TMapSeq2[T1, T2](that: Seq[(T1, T2)]) {
  //   def tmap[R](f: (T1, T2) => R): Seq[R] = that map f.tupled
  //   def tforall(f: (T1, T2) => Boolean): Boolean = that forall f.tupled
  // }

}
