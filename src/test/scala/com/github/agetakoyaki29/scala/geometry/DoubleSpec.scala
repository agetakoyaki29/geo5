package com.github.agetakoyaki29.scala.geometry

import org.scalatest.WordSpec


abstract class DoubleSpec extends WordSpec {
  val NaN = Double.NaN
  val PositiveInfinity = Double.PositiveInfinity
  val NegativeInfinity = Double.NegativeInfinity
  val MinPositiveValue = Double.MinPositiveValue
  val MinNormal = Delta.MinNormal  // java.lang.Double.MIN_NORMAL
  val MaxValue = Double.MaxValue
  val MinValue = Double.MinValue
  val PositiveZero = +0d
  val NegativeZero = -0d
  val One = 1d
  val PositiveDouble = +313.523
  val NegativeDouble = -62.66

  def dump(double: Double): String = s"double($double)"

}
