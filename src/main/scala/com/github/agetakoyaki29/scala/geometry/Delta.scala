package com.github.agetakoyaki29.scala.geometry


object Delta {

  // object Default {
    implicit val delta: Double = MyDouble.MinNormal.scalb(6)
  // }

  def relativeDelta(d: Double): Double = d.ulp.scalb(6)

  def eq0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = {
    d1.abs <= (delta +: deltas).max
  }
  def eq(d1: Double, d2: Double): Boolean = eq0(d1-d2, d1.relativeDelta, d2.relativeDelta)
  // def sumeq0(ds: Seq[Double]): Boolean = eq0(ds.sum, ds.map{_.relativeDelta}: _*)

  def lt0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = d1 <=  (delta +: deltas).max
  def lt(d1: Double, d2: Double): Boolean = lt0(d1-d2, d1.relativeDelta, d2.relativeDelta)

  def gt0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = d1 >= -(delta +: deltas).max
  def gt(d1: Double, d2: Double): Boolean = gt0(d1-d2, d1.relativeDelta, d2.relativeDelta)

}
