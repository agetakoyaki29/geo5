package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


class Figure2Test extends Figure2Spec {

  // for(figure <- figures) Tests(figure)

  def Tests(it: Figure2) = {
    dump(it) should {
      // figure
      "it.aabb contain it" in (pending)
      "it.aabb is intersect it" in (pending)
      "it same it" in (pending)
      "it contain it" in (pending)
      "it is not intersect it" in (pending)
      // figure, pt
      "it nearest any pt norm === it distance any pt" in (pending)
      "it through (it nearest any pt)" in (pending)
      // figure, figure
      "it through (it intersect any figure)" in (pending)
    }

    dump(it) when {
      // figure, pt
      "it through a pt" should {
        val thes = pts filter {it through _}
        "it contain the pt" in (pending)
      }
      // figure, figure
      "it is not intersect a figure" should {
        val thes = figures filterNot {it isIntersect _}
        "it intersect the figure have size 0" in (pending)
      }
      "it is intersect a figure" should {
        val thes = figures filter {it isIntersect _}
        "it intersect the figure don't have size 0" in (pending)
      }
      "it same a figure" should {
        val thes = figures filter {it same _}
        "it not intersect the figure" in (pending)
      }
    }
  }

}
