package com.github.agetakoyaki29.scala.geometry


object MyDouble {

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
