package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.geometry


trait Figure2 {
  // def aabb: AABB2
  // def distance(pt: Point2): Double
  // def nearest(pt: Point2): Point2
  // def through(pt: Point2): Boolean = this intersect pt

  // def same(op: Figure2): Boolean
  // def contain(op: Figure2): Boolean

  /**
   * this is not intersect this ? {this intersect this => Seq()}
   */
  def isIntersect(op: Figure2): Boolean
  def intersect(op: Figure2): Set[Point2]
}
