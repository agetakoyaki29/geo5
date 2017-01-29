package com.github.agetakoyaki29.scala.geometry


class DoubleTest extends DoubleSpec {

  "NaN is NaN" in {
    assert(NaN isNaN)
  }
  "PositiveInfinity is Infinite" in {
    assert(PositiveInfinity isInfinite)
    assert(PositiveInfinity isPositiveInfinite)
  }
  "NegativeInfinity is Infinite" in {
    assert(NegativeInfinity isInfinite)
    assert(NegativeInfinity isNegativeInfinite)
  }
  "PositiveZero is Zero" in {
    assert(PositiveZero isZero)
    assert(PositiveZero isPositiveZero)
  }
  "NegativeZero is Zero" in {
    assert(NegativeZero isZero)
    assert(NegativeZero isNegativeZero)
  }

  // isPos, Neg

  "PositiveInfinity is opposite NegativeInfinity (but add => NaN)" in {
    assert(PositiveInfinity === -NegativeInfinity)
    assert(NegativeInfinity === -PositiveInfinity)
    assert(PositiveInfinity + NegativeInfinity isNaN)
  }
  "PositiveZero is opposite NegativeZero" in {
    assert(PositiveZero === -NegativeZero)
    assert(NegativeZero === -PositiveZero)
    assert(PositiveZero + NegativeZero isZero)
  }
  "MaxValue is opposite MinValue" in {
    assert(MaxValue === -MinValue)
    assert(MinValue === -MaxValue)
    assert(MaxValue + MinValue isZero)
  }

  "overflow" when {
    "MaxValue * 2 is infinite" in {
      assert(MaxValue * 2 isPositiveInfinite)
      assert(MinValue * 2 isNegativeInfinite)
    }
    "MaxValue nextUp is infinite" in {
      assert(MaxValue.nextUp isPositiveInfinite)
      assert(MinValue.nextDown isNegativeInfinite)
    }
  }
  "underflow" when {
    "MinPositiveValue / 2 is zero" in {
      assert(MinPositiveValue / 2 isZero)
    }
  }

  "1 / MinNormal <= MaxValue" in {
    assert(1 / MinNormal <= MaxValue)
  }
  "1 / MinPositiveValue is infinite" in {
    assert(1 / MinPositiveValue isInfinite)
  }

}
