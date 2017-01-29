package com.github.agetakoyaki29.scala.geometry.dim2.point

import org.scalatest.Matchers

import com.github.agetakoyaki29.scala.geometry._


class Figure2Test extends Figure2Spec with Matchers with DeltaMatchers {

  for(figure <- figures) Tests(figure)

  def Tests(it: Figure2) = {
    dump(it) should {
      // figure
      // "it.aabb contain it" in {it.aabb should contain (it)}
      // "it.aabb is intersect it" in {it.aabb should isIntersect (it)}
      "it same it" in {it should same (it)}
      "it contain it" in {it should contain (it)}
      "it is not intersect it" in {it should isNotIntersect (it)}
      // figure, pt
      "it nearest any pt to any pt norm =~ it distance any pt" in {for(any <- pts) (it nearest any to any norm) should =~ (it distance any)}
      "it through (it nearest any pt)" in {for(any <- pts) it should through (it nearest any)}
      // figure, figure
      "it through (it intersect any figure)" in {for(any <- figures)
        for(intersect <- it intersect any) it should through (intersect)}
    }

    dump(it) when {
      // figure, pt
      "it through a pt" should {
        val thes = pts filter {it through _}
        "it contain the pt" in {for(the <- thes) it should contain (the)}
      }
      // figure, figure
      "it is not intersect a figure" should {
        val thes = figures filterNot {it isIntersect _}
        "it intersect the figure is empty" in {for(the <- thes) it intersect the should be (empty)}
      }
      "it is intersect a figure" should {
        val thes = figures filter {it isIntersect _}
        "it intersect the figure is not empty" in {for(the <- thes) it intersect the should not be empty}
      }
      "it same a figure" should {
        val thes = figures filter {it same _}
        "it is not intersect the figure" in {for(the <- thes) it should isNotIntersect (the)}
      }
    }
  }

}
