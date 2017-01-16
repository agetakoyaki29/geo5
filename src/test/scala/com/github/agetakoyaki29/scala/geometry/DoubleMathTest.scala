package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


class DoubleMathTest extends DoubleSpec {

  "Math.signum: Double === -0d" in {
    assert(Math.signum(NegativeZero) === -0d)
  }
  "signum: Int === 0" in {
    assert(NegativeZero.signum === 0)
  }

}
