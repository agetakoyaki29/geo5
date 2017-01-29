package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}


trait Dim extends IndexedSeq[Double] {

  // val factory: DimFactory[_ <: Dim]

  final def isZero: Boolean = map{_.isZero}.forall(identity)
  final def isInfinite: Boolean = !isNaN && map{_.isInfinite}.exists(identity)
  final def isNaN: Boolean = map{_.isNaN}.exists(identity)

  def norm: Double = normSqr.sqrt
  /**
   * this map {d => d*d} reduce {_+_}
   * this dot this
   */
  def normSqr: Double = this map {d => d*d} sum

  // -- std --

  override def toString = this.getClass.getSimpleName + "(" + mkString(", ") + ")"  // for convenience

}
