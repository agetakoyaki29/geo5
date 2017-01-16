package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


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

}
