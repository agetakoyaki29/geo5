package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


class DoubleTest extends DoubleSpec {

  "NaN is NaN" in {
    assert(NaN isNaN)
  }
  "PositiveInfinity is Infinite" in {
    assert(PositiveInfinity isInfinite)
  }
  "NegativeInfinity is Infinite" in {
    assert(NegativeInfinity isInfinite)
  }
  "PositiveZero is Zero" in {
    assert(PositiveZero isZero)
  }
  "NegativeZero is Zero" in {
    assert(NegativeZero isZero)
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
