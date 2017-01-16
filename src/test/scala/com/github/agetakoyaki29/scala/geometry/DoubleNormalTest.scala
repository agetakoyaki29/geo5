package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


class DoubleNormalTest extends DoubleSpec {
  val double1 = 2d
  val double2 = Math.PI
  val double3 = -0.524d
  val minP1 = Double.MinPositiveValue.scalb(4)
  val NonNormal1 = MinNormal.nextDown

  s"NaN($NaN)" should {
    behave like testUlp(NaN)
    behave like testNormal(NaN)
  }
  s"PositiveInfinity($PositiveInfinity)" should {
    behave like testUlp(PositiveInfinity)
    behave like testNormal(PositiveInfinity)
  }
  s"NegativeInfinity($NegativeInfinity)" should {
    behave like testUlp(NegativeInfinity)
    behave like testNormal(NegativeInfinity)
  }
  s"PositiveZero($PositiveZero)" should {
    behave like testUlp(PositiveZero)
    behave like testNormal(PositiveZero)
  }
  s"NegativeZero($NegativeZero)" should {
    behave like testUlp(NegativeZero)
    behave like testNormal(NegativeZero)
  }
  s"MinPositiveValue($MinPositiveValue)" should {
    "ulp === Double.MinPositiveValue" in {
      assert(MinPositiveValue.ulp === Double.MinPositiveValue)
    }
    behave like testUlp(MinPositiveValue)
    behave like testNormal(MinPositiveValue)
  }
  s"MinNormal($MinNormal)" should {
    "ulp === Double.MinPositiveValue" in {
      assert(MinNormal.ulp === Double.MinPositiveValue)
    }
    behave like testUlp(MinNormal)
    behave like testNormal(MinNormal)
  }
  s"MaxValue($MaxValue)" should {
    behave like testUlp(MaxValue)
    behave like testNormal(MaxValue)
  }
  s"MinValue($MinValue)" should {
    behave like testUlp(MinValue)
    behave like testNormal(MinValue)
  }

  s"double1($double1)" should {
    behave like testUlp(double1)
    behave like testNormal(double1)
  }
  s"double2($double2)" should {
    behave like testUlp(double2)
    behave like testNormal(double2)
  }
  s"double3($double3)" should {
    behave like testUlp(double3)
    behave like testNormal(double3)
  }
  s"minP1($minP1)" should {
    behave like testUlp(minP1)
    behave like testNormal(minP1)
  }
  s"NonNormal1($NonNormal1)" should {
    behave like testUlp(NonNormal1)
    behave like testNormal(NonNormal1)
  }

  def testNormal(double: Double) = double match {
    case _ if !double.isNormal => {
      "non normal double !isNaN && !isInf && !isZero" in {
        assert(!(double.isNaN || double.isInfinite || double.isZero))
      }
      "non normal double abs < java.lang.Double.MIN_NORMAL" in {
        assert(double.abs < java.lang.Double.MIN_NORMAL)
      }
    }
    case _ if double.isNormal => double match {
      case _ if double.isNaN =>
        "it getExponent === java.lang.Double.MAX_EXPONENT + 1" in {
          assert(double.getExponent === (java.lang.Double.MAX_EXPONENT+1))
        }
      case _ if double.isInfinite =>
        "it getExponent === java.lang.Double.MAX_EXPONENT + 1" in {
          assert(double.getExponent === (java.lang.Double.MAX_EXPONENT+1))
        }
      case _ if double.isZero =>
        "it getExponent === java.lang.Double.MIN_EXPONENT - 1" in {
          assert(double.getExponent === (java.lang.Double.MIN_EXPONENT-1))
        }
      case _=>
        "normal double abs >= java.lang.Double.MIN_NORMAL" in {
          assert(double.abs >= java.lang.Double.MIN_NORMAL)
        }
        "normal double.getExponent === double.abs.log2.rint" ignore {
          assert(double.getExponent === double.abs.log2.rint)
        }
    }
  }

  def testUlp(it: Double) = it match {
    case _ if it.isNaN =>
      "it.ulp isNaN" in {
        assert(it.ulp.isNaN)
      }
    case _ if it.isInfinite =>
      "it.ulp === Double.PositiveInfinity" in {
        assert(it.ulp === Double.PositiveInfinity)
      }
    case _ =>
      "it + it.ulp !== it" in {
        assert(it + it.ulp !== it)
      }
      "it + it.ulp.scalb(-1) === it" in {
        assert(it + it.ulp.scalb(-1) === it)
      }
      "it + it.ulp === it.nextUp" in {
        assert(it + it.ulp === it.nextUp)
      }
      "it - it.ulp === it.nextDown" in {
        assert(it - it.ulp === it.nextDown)
      }
      "it.ulp > 0" in {
        assert(it.ulp > 0)
      }
      "it.ulp === (-it).ulp" in {
        assert(it.ulp === (-it).ulp)
      }
  }

}
