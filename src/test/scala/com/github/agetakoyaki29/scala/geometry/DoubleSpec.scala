package com.github.agetakoyaki29.scala.geometry

import org.scalatest.WordSpec


abstract class DoubleSpec extends WordSpec {
  val NaN = Double.NaN

  val PositiveInfinity = Double.PositiveInfinity
  val NegativeInfinity = Double.NegativeInfinity

  val Zero = MyDouble.Zero
  val PositiveZero = MyDouble.PositiveZero
  val NegativeZero = MyDouble.NegativeZero

  val MaxValue = Double.MaxValue
  val MinValue = Double.MinValue
  val MinPositiveValue = Double.MinPositiveValue

  val MinNormal = MyDouble.MinNormal  // java.lang.Double.MIN_NORMAL

  // val One = 1d

  val PositiveDouble = +313.523
  val NegativeDouble = -62.66

  def dump(double: Double): String = s"double($double)"

}
