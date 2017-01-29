package com.github.agetakoyaki29.scala.geometry


object Delta {

  // object Default {
    implicit val delta: Double = MinNormal.scalb(6)
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

  // ---- Math ----

  val MaxExponent = java.lang.Double.MAX_EXPONENT
  val MinExponent = java.lang.Double.MIN_EXPONENT

  // def sign(boolean: Boolean): Int = if(boolean) 1 else -1

  def minus(double: Double): Double = -double

  def isPositive(double: Double): Boolean = double.toLongBits >>> 63 == 0
  def isNegative(double: Double): Boolean = ! isPositive(double)

  def isPositiveInfinite(double: Double): Boolean = double.isPositive && double.isInfinite
  def isNegativeInfinite(double: Double): Boolean = double.isNegative && double.isInfinite

  def log2(double: Double): Double = log(double, 2)   // FIXME fast, rounded, check special double

  def log(d1: Double, d2: Double): Double = Math.log(d1) / Math.log(d2)   // FIXME fast, rounded, check special double

  // ---- Zero ----

  val Zero = 0d
  val PositiveZero = +0d
  val NegativeZero = -0d

  def isZero(double: Double): Boolean = double == 0d
  def isPositiveZero(double: Double): Boolean = double.isZero && double.isPositive
  def isNegativeZero(double: Double): Boolean = double.isZero && double.isNegative

  // ---- Normal ----

  lazy val MinNormal = java.lang.Double.MIN_NORMAL

  def isNormal(double: Double): Boolean = double match {
    case _ if double.isNaN => true
    case _ if double.isInfinite => true
    case _ if double.isZero => true
    case _ => double.abs >= MinNormal
  }

  // ---- radian degree ----

  // def chompRadian(double: Double): Double = ???
  // def chompDegree(double: Double): Double = ???

  // ---- for validation ----

  val NotNaN:      Double => Double = d => { require(! d.isNaN,      "not NaN Double");      d }
  val NotInfinite: Double => Double = d => { require(! d.isInfinite, "not Infinite Double"); d }
  val NotZero:     Double => Double = d => { require(! d.isZero,     "not Zero Double");     d }
  val NotPlus:     Double => Double = d => { require(!(d > 0),       "not Plus Double");     d }
  val NotMinus:    Double => Double = d => { require(!(d < 0),       "not Minus Double");    d }
  val Identity:    Double => Double = identity

}
