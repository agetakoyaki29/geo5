package com.github.agetakoyaki29.scala.geometry.dim2

import org.scalatest.WordSpec

import com.github.agetakoyaki29.scala.geometry.dim2.point._


abstract class Dim2Spec extends WordSpec {
  val Zero = Dim2.Zero
  val E0 = Dim2.E(0)
  val E1 = Dim2.E(1)

  val dim1 = Dim2(63.156, 3.4121)             // not NaN, Inf
  val vector1 = Vector2(-24.422, 466.34)
  val point1 = Point2(-643.88, 05.76)
  val dir1 = Dir2(83.594, -43.896)            // not Zero
  val range1 = Range2(-843.54684, -3.456841)  // not Zero
  val corner1 = Corner2(13463.46, 3545.38)    // be Positive

}
