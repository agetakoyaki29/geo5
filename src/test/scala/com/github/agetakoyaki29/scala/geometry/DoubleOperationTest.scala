package com.github.agetakoyaki29.scala.geometry

import org.scalatest.Matchers


class DoubleOperationTest extends DoubleSpec with Matchers {

  "about ==,!=,<,<=,>,>=(6 comparison operations)" when {
    "NaN !== Nan" in {
      assert(NaN !== NaN)
    }
    "+Zero === -Zero" in {
      assert(PositiveZero === NegativeZero)
    }
  }

  "about +,-,*,/,%(5 arithmetic operations)" when {
    "Inf + Inf === Inf (same sign)" in {
      assert(PositiveInfinity + PositiveInfinity === PositiveInfinity)
      assert(NegativeInfinity + NegativeInfinity === NegativeInfinity)
    }
    "Inf + Inf is NaN (different sign)" in {
      assert((NegativeInfinity + PositiveInfinity).isNaN)
      assert((PositiveInfinity + NegativeInfinity).isNaN)
    }

    "Inf * Zero is NaN (and vise versa)" in {
      assert((PositiveInfinity * PositiveZero).isNaN)
      assert((NegativeInfinity * NegativeZero).isNaN)
      assert((PositiveInfinity * NegativeZero).isNaN)
      assert((NegativeInfinity * PositiveZero).isNaN)
      assert((PositiveZero * PositiveInfinity).isNaN)
      assert((NegativeZero * NegativeInfinity).isNaN)
      assert((NegativeZero * PositiveInfinity).isNaN)
      assert((PositiveZero * NegativeInfinity).isNaN)
    }

    "Inf / Inf isNaN" in {
      assert((PositiveInfinity / PositiveInfinity).isNaN)
      assert((NegativeInfinity / NegativeInfinity).isNaN)
      assert((PositiveInfinity / NegativeInfinity).isNaN)
      assert((NegativeInfinity / PositiveInfinity).isNaN)
    }
    "any(non Inf) / Inf === Zero" in {
      assert((PositiveDouble / PositiveInfinity).isPositiveZero)
      assert((NegativeDouble / NegativeInfinity).isPositiveZero)
      assert((PositiveDouble / NegativeInfinity).isNegativeZero)
      assert((NegativeDouble / PositiveInfinity).isNegativeZero)

      assert((PositiveZero / PositiveInfinity).isPositiveZero)
      assert((NegativeZero / NegativeInfinity).isPositiveZero)
      assert((PositiveZero / NegativeInfinity).isNegativeZero)
      assert((NegativeZero / PositiveInfinity).isNegativeZero)
    }
    "Zero / Zero isNaN" in {
      assert((PositiveZero / PositiveZero).isNaN)
      assert((NegativeZero / NegativeZero).isNaN)
      assert((PositiveZero / NegativeZero).isNaN)
      assert((NegativeZero / PositiveZero).isNaN)
    }
    "any(non Zero) / Zero === Double.PositiveInfinity (same sign)" in {
      assert(PositiveDouble / PositiveZero === PositiveInfinity)
      assert(NegativeDouble / NegativeZero === PositiveInfinity)
      assert(PositiveInfinity / PositiveZero === PositiveInfinity)
      assert(NegativeInfinity / NegativeZero === PositiveInfinity)
    }
    "any(non Zero) / Zero === Double.NegativeInfinity (different sign)" in {
      assert(PositiveDouble / NegativeZero === NegativeInfinity)
      assert(NegativeDouble / PositiveZero === NegativeInfinity)
      assert(PositiveInfinity / NegativeZero === NegativeInfinity)
      assert(NegativeInfinity / PositiveZero === NegativeInfinity)
    }
  }

}
