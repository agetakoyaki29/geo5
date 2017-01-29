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
  def distance(pt: Point2): Double = {
    if(this through pt) 0
    else (this cross pt / this.norm).abs
  }
  def distanceSqr(pt: Point2): Double = {
    if(this through pt) 0
    else (this cross pt).sqr / this.normSqr
  }

  /**
   * this.normalized * this cosTo pt * pt.norm
   * pt + this.normal.normalized * -distance
   * this * (this dot pt) / (this dot this)
   */
  def nearest(pt: Point2): Point2 = {
    if(this through pt) pt
    else Point2(this) * (this dot pt / this.normSqr)
  }

  // ---- figure to other figure ----

  def points: Set[Point2] = Set(O, Point2(this))

  def aabb: AABB2 = AABB2.Whole

  def sameDir2(op: Dir2): Boolean = this parallel op
  def sameLine2(line: Line2): Boolean = (this through line.sp) && (this sameDir2 line.dir)

  def isIntersectLine2(line: Line2): Boolean = ! (this parallel line.dir)
  def intersectLine2(line: Line2): Set[Point2] = intersectTimeLine2(line) map {Point2(this) * _} toSet
  def intersectTimeLine2(line: Line2): Set[Double] = {
    if(!(this isIntersectLine2 line)) Set()
    else Set( (line.sp cross line.dir) / (this cross line.dir) )
  }

  def isIntersectAABB2(aabb: AABB2): Boolean = false  // TODO
  def intersectAABB2(aabb: AABB2): Set[Point2] = intersectTimeAABB2(aabb) map {Point2(this) * _} toSet
  def intersectTimeAABB2(aabb: AABB2): Set[Double] = Set() // TODO

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

  def same(figure: Figure2): Boolean = figure match {
    case line: Line2 => this sameLine2 line
    case _ => false
  }

  def contain(op: Figure2): Boolean = op match {
    case line: Line2 => !(this isIntersect line) && (this containPoint2 line.sp)
    case circle: Circle2 => (this containPoint2 circle.center) && ((this distanceSqr circle.center) <~ circle.powerSqr)
    case aabb: AABB2 => aabb.points map {this containPoint2 _} forall identity
  }

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

  // ---- std ----

  override def toString: String = s"Line2($sp, $dir)"

  override def equals(op: Any) = op match {
    case line: Line2 => sp == line.sp && dir == line.dir
    case _ => false
  }

  override def hashCode: Int = 32*sp.## + dir.##

  // ----

  def normalDir: Line2 = normalDir(O)
  def normalDir(at: Point2): Line2 = Line2(this nearest at, dir.normalDir)

  def normal(op: Line2): Boolean = sp conjugate dir.normal apply op.dir
  def parallel(op: Line2): Boolean = sp conjugate dir.parallel apply op.dir
  def angleTo(op: Line2): Double = sp conjugate dir.angleTo apply op.dir
  def cosTo(op: Line2): Double = sp conjugate dir.cosTo apply op.dir
  def sinTo(op: Line2): Double = sp conjugate dir.sinTo apply op.dir

  // ---- copy from Dir2 ----

  def align(idx: Int): Boolean = sp conjugate dir.align apply idx

  def angle: Double = sp unto dir.angle

  def inRegion1(pt: Point2): Boolean = sp conjugate dir.inRegion1 apply pt
  def inRegion2(pt: Point2): Boolean = sp conjugate dir.inRegion2 apply pt
  def through(pt: Point2): Boolean = sp conjugate dir.through apply pt
  def containPoint2(pt: Point2): Boolean = sp conjugate dir.containPoint2 apply pt
  def distance(pt: Point2): Double = sp conjugate dir.distance apply pt
  def distanceSqr(pt: Point2): Double = sp conjugate dir.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = sp conjugate dir.nearest apply pt

  def points: Set[Point2] = dir.points map {_ unfrom sp}
  def aabb: AABB2 = sp unto dir.aabb
  def sameLine2(line: Line2): Boolean = sp conjugate dir.sameLine2 apply line
  def isIntersectLine2(line: Line2): Boolean = sp conjugate dir.isIntersectLine2 apply line
  def isIntersectAABB2(aabb: AABB2): Boolean = sp conjugate dir.isIntersectAABB2 apply aabb
  def intersectLine2(line: Line2): Set[Point2] = dir.intersectLine2(line from sp) map {_ unfrom sp}
  def intersectAABB2(aabb: AABB2): Set[Point2] = dir.intersectAABB2(aabb from sp) map {_ unfrom sp}
}
