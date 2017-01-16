package com.github.agetakoyaki29.scala.geometry.dim2.point

import org.scalatest.WordSpec


class Figure2Spec extends WordSpec {
  val O = Point2.O

  val point1 =  Point2 ( 414.55,  9843.2)
  val dir1 =    Dir2   (-68.844, -3.5487)
  val range1 =  Range2 (-683.1,   53.487)
  val corner1 = Corner2( 867.879, 486.84)

  val line1 =   Line2  (Point2( 32.3152, -564.656), Dir2   (97.687,   68.77))
  val circle1 = Circle2(Point2(-9843.964, 5.6876),  Range2 (644.446, -78.6877))
  val aabb1 =   AABB2  (Point2( 57.84,    687.97),  Corner2(498.88,   49684.68))

  val pts = Seq()
  val figures = Seq()

  def dump(figure: Figure2): String = ???

}
