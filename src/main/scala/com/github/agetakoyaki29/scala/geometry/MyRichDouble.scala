package com.github.agetakoyaki29.scala.geometry


class MyRichDouble(val that: Double) {
  def sqr: Double = this.pow(2)

  // ---- copy from Delta ----
  def relativeDelta: Double = Delta.relativeDelta(that)
  def =~(op: Double): Boolean = Delta.eq(that, op)
  def >~(op: Double): Boolean = Delta.gt(that, op)
  def <~(op: Double): Boolean = Delta.lt(that, op)

  // ---- copy from MyDouble ----
  def minus: Double = MyDouble.minus(that)
  def isPositive: Boolean = MyDouble.isPositive(that)
  def isNegative: Boolean = MyDouble.isNegative(that)
  def isPositiveInfinite: Boolean = MyDouble.isPositiveInfinite(that)
  def isNegativeInfinite: Boolean = MyDouble.isNegativeInfinite(that)
  def isPositiveZero: Boolean = MyDouble.isPositiveZero(that)
  def isNegativeZero: Boolean = MyDouble.isNegativeZero(that)
  def isZero: Boolean = MyDouble.isZero(that)
  def isNormal: Boolean = MyDouble.isNormal(that)
  def log2: Double = MyDouble.log2(that)
  def log(d2: Double): Double = MyDouble.log(that, d2)

  // ---- copy from java.lang.Double ----
  def isFinite: Boolean = java.lang.Double.isFinite(that)
  def toLongBits: Long = java.lang.Double.doubleToLongBits(that)
  def toRawLongBits: Long = java.lang.Double.doubleToRawLongBits(that)
  def toHexString: String = java.lang.Double.toHexString(that)

  // ---- copy from Math ----
  // acos, asin, atan, atan2, cos, cosh, exp, expm1, hypot, sin, sinh, tan, tanh
  // ----
  def ulp: Double = Math.ulp(that)
  def nextUp: Double = Math.nextUp(that)
  def nextDown: Double = Math.nextDown(that)
  def nextAfter(direction: Double): Double = Math.nextAfter(that, direction)
  // ----
  def cbrt: Double = Math.cbrt(that)
  def sqrt: Double = Math.sqrt(that)
  // ----
  def copySign(sign: Double): Double = Math.copySign(that, sign)
  def getExponent: Double = Math.getExponent(that)
  def IEEEremainder(divisor: Double): Double = Math.IEEEremainder(that, divisor)
  def log: Double = Math.log(that)
  def log10: Double = Math.log10(that)
  def log1p: Double = Math.log1p(that)
  def pow(exponent: Double): Double = Math.pow(that, exponent)
  def rint: Double = Math.rint(that)
  def scalb(scaleFactor: Int): Double = Math.scalb(that, scaleFactor)
}


// for long bits
class LongBits(val that: Long) {
  def bitsToDouble: Double = java.lang.Double.longBitsToDouble(that)
}


// class RichBoolean(val that: Boolean) {
//   def sign: Int = Delta.sign(that)
// }
