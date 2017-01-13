package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.dim2.{Dim2Factory, Dim2, Vector2}


object Dir2 extends Dim2Factory[Dir2] {
  def apply(x: Double, y: Double): Dir2 = new Dir2(x, y)

  def AtAngle(deg: Double): Dir2 = this(Math.cos(deg), Math.sin(deg))
}


@SameRet
class Dir2(_x: Double, _y: Double) extends Vector2(_x, _y) {

  override val factory: Dim2Factory[_ <: Dir2] = Dir2

  def toDir2: Dir2 = this

  override protected def validate = Dim2.NotZero andThen super.validate

  // ----

  @UpRet def reflect: Dir2 = minus

  // ---- about angle ----

  def align(idx: Int): Boolean = this parallel Dir2.E(idx)

  @UpRet def normalDir: Dir2 = factory(-y, x)

  def normal(op: Dir2): Boolean = this dotEq0 op
  def parallel(op: Dir2): Boolean = this crossEq0 op

  /**
   * -pi ~ pi
   */
  def angle: Double = Math.atan2(y, x)

  def angleTo(op: Dir2): Double = op.angle - this.angle

  def cosTo(op: Dir2): Double = this dot op / this.norm / op.norm

  def sinTo(op: Dir2): Double = this cross op / this.norm / op.norm

  // ---- figure to point ----

  def inRegion1(pt: Point2): Boolean = this dotGt0 pt
  def inRegion2(pt: Point2): Boolean = (this reflect) inRegion1 (pt - this)

  def through(pt: Point2): Boolean = this crossEq0 pt

  /**
   * 0 <= this angle pt <= pi
   * (this sinTo pt) < 0
   */
  def containPoint2(pt: Point2): Boolean = this crossLt0 pt

  /**
   * this sinTo pt * pt.norm
   */
  def distance(pt: Point2): Double = (this cross pt / this.norm).abs
  def distanceSqr(pt: Point2): Double = (this cross pt).sqr / this.normSqr

  /**
   * this.normalized * this cosTo pt * pt.norm
   * pt + this.normal.normalized * -distance
   * this * (this dot pt) / (this dot this)
   */
  def nearest(pt: Point2): Point2 = Point2(this * (this dot pt / this.normSqr))

  // ---- figure to other figure ----

  def same(op: Dir2): Boolean = this parallel op

  def same(line: Line2): Boolean = (this through line.sp) && (this same line.dir)

  def aabb: AABB2 = AABB2.Whole

  def isIntersectLine2(line: Line2): Boolean = ! (this parallel line.dir)
  def intersectLine2(line: Line2): Set[Point2] = intersectTimeLine2(line) map {Point2(this) * _} toSet
  def intersectTimeLine2(line: Line2): Set[Double] = {
    if(!(this isIntersectLine2 line)) Set()
    else Set( (line.sp cross line.dir) / (this cross line.dir) )
  }

  def isIntersectAABB2(aabb: AABB2): Boolean = ???
  def intersectAABB2(aabb: AABB2): Set[Point2] = intersectTimeAABB2(aabb) map {Point2(this) * _} toSet
  def intersectTimeAABB2(aabb: AABB2): Set[Double] = ???

  // ---- UpRet ----

  override def reverseD2 = factory(super.reverseD2)
  override def updatedD2(idx: Int, elem: Double) = factory(super.updatedD2(idx, elem))
  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+())
  override def unary_-() = factory(super.unary_-())
  override def +(op: Dim2) = factory(super.+(op))
  override def -(op: Dim2) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))
  override def minus = factory(super.minus)
  override def normalized = factory(super.normalized)
}


object Line2 {
  def apply(sp: Point2, dir: Dir2) = new Line2(sp, dir)
  def apply(sp: Point2, ep: Point2) = new Line2(sp, Dir2(sp to ep))

  implicit def dir2ToLine2(dir: Dir2): Line2 = Line2(O, dir)
}


class Line2(val sp: Point2, val dir: Dir2) extends Trans2[Line2] with Figure2 {
  val ep = sp unto Point2(dir)

  def updated(sp: Point2, dir: Dir2): Line2 = Line2(sp, dir)
  def updatedSP(sp: Point2): Line2 = updated(sp, dir)
  def updatedDir(dir: Dir2): Line2 = updated(sp, dir)
  def updatedEP(ep: Point2): Line2 = updatedDir(Dir2(sp to ep))

  def reflect: Line2 = Line2(sp unto Point2(dir), dir.reflect)

  // ---- for Trans2 ----

  def +(op: Point2): Line2 = updatedSP(sp+op)
  def -(op: Point2): Line2 = updatedSP(sp-op)

  // ---- for Figure2 ----

  def isIntersect(op: Figure2): Boolean = op match {
    case line: Line2 => this isIntersectLine2 line
    case aabb: AABB2 => this isIntersectAABB2 aabb
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Set[Point2] = op match {
    case line: Line2 => this intersectLine2 line
    case aabb: AABB2 => this intersectAABB2 aabb
    case _ => op intersect this
  }

  // ---- copy from Dir2 ----

  def align(idx: Int): Boolean = dir.align(idx)
  def normalDir: Line2 = sp unto dir.normalDir
  def normal(op: Line2): Boolean = dir.normal(op.dir)
  def parallel(op: Line2): Boolean = dir.parallel(op.dir)

  def angle: Double = dir.angle
  def angleTo(op: Line2): Double = dir.angleTo(op.dir)
  def cosTo(op: Line2): Double = dir.cosTo(op.dir)
  def sinTo(op: Line2): Double = dir.sinTo(op.dir)

  def inRegion1(pt: Point2): Boolean = dir.inRegion1(sp to pt)
  def inRegion2(pt: Point2): Boolean = dir.inRegion2(sp to pt)
  def through(pt: Point2): Boolean = dir.through(sp to pt)
  def containPoint2(pt: Point2): Boolean = dir.containPoint2(sp to pt)
  def distance(pt: Point2): Double = dir.distance(sp to pt)
  def distanceSqr(pt: Point2): Double = dir.distanceSqr(sp to pt)
  def nearest(pt: Point2): Point2 = sp unto dir.nearest(sp to pt)

  def same(line: Line2): Boolean = dir.same(sp to line)
  def aabb: AABB2 = sp unto dir.aabb
  def isIntersectLine2(line: Line2): Boolean = dir.isIntersectLine2(sp to line)
  def isIntersectAABB2(aabb: AABB2): Boolean = dir.isIntersectAABB2(sp to aabb)
  def intersectLine2(line: Line2): Set[Point2] = dir.intersectLine2(sp to line) map {sp unto _}
  def intersectAABB2(aabb: AABB2): Set[Point2] = dir.intersectAABB2(sp to aabb) map {sp unto _}
}
