package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.dim2.{Dim2Factory, Dim2, Vector2}


object Range2 extends Dim2Factory[Range2] {
  def apply(x: Double, y: Double): Range2 = new Range2(x, y)
}


@SameRet
class Range2(_x: Double, _y: Double) extends Vector2(_x, _y) {

  override val factory: Dim2Factory[_ <: Range2] = Range2

  def toRange2: Range2 = this

  override protected def validate = Dim2.NotZero andThen super.validate

  // ----

  def power: Double = this.norm
  def powerSqr: Double = this.normSqr

  def center: Point2 = O

  def isConcentric(circle: Circle2): Boolean = center samePoint2 circle.center

  def radicalLine(circle: Circle2): Line2 = {
    require(!(this isConcentric circle), "not concentric Circle2")
    val opcenter = circle.center
    val radicalPoint = opcenter * ( ((this.powerSqr-circle.powerSqr)/opcenter.normSqr + 1) / 2 )
    Line2(radicalPoint, Dir2(opcenter).normalDir)
  }

  // ---- figure to point ----

  def through(pt: Point2): Boolean = this.normSqr =~ pt.normSqr

  def containPoint2(pt: Point2): Boolean = this.normSqr >~ pt.normSqr

  def distance(pt: Point2): Double = (pt.norm - this.norm).abs
  def distanceSqr(pt: Point2): Double = distance(pt).sqr

  def nearest(pt: Point2): Point2 = pt * (this.norm / pt.norm)

  // ---- figure to other figure ----

  def sameRange2(op: Range2): Boolean = this.powerSqr =~ op.powerSqr

  def sameCircle2(circle: Circle2): Boolean = (this isConcentric circle) && (this sameRange2 circle.range)

  def aabb: AABB2 = AABB2(O, Corner2(norm, norm))

  def isIntersectLine2(line: Line2): Boolean = this.powerSqr >~ (line distanceSqr O)
  def intersectLine2(line: Line2): Set[Point2] = {
    val nearest = line nearest O
    if(!(this containPoint2 nearest)) Set()
    else if(this through nearest) Set(nearest)  // not need if Set work correctly
    else {
      val sine = (this.normSqr - nearest.normSqr).sqrt
      val diff = line.dir.normalized * sine
      Set(nearest+diff, nearest-diff)
    }
  }

  def isIntersectCircle2(circle: Circle2): Boolean = (this.power + circle.power) >~ circle.center.norm
  def intersectCircle2(circle: Circle2): Set[Point2] = {
    if(this isConcentric circle) Set()
    else this intersectLine2 (this radicalLine circle)
  }

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


object Circle2 {
  def apply(sp: Point2, range: Range2) = new Circle2(sp, range)
  def apply(sp: Point2, ep: Point2) = new Circle2(sp, Range2(sp to ep))

  implicit def range2ToCircle2(range: Range2): Circle2 = Circle2(O, range)
}


class Circle2(val sp: Point2, val range: Range2) extends Trans2[Circle2] with Figure2 {
  val ep = sp unto Point2(range)

  def updated(sp: Point2, range: Range2): Circle2 = Circle2(sp, range)
  def updatedSP(sp: Point2): Circle2 = updated(sp, range)
  def updatedRange(range: Range2): Circle2 = updated(sp, range)
  def updatedEP(ep: Point2): Circle2 = updatedRange(Range2(sp to ep))

  // ---- for Trans2 ----

  def +(op: Point2): Circle2 = updatedSP(sp+op)
  def -(op: Point2): Circle2 = updatedSP(sp-op)

  // ---- for Figure2 ----

  def same(any: Any): Boolean = any match {
    case circle: Circle2 => this sameCircle2 circle
    case _ => false
  }

  def isIntersect(op: Figure2): Boolean = op match {
    case line: Line2 => this isIntersectLine2 line
    case circle: Circle2 => this isIntersectCircle2 circle
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Set[Point2] = op match {
    case line: Line2 => this intersectLine2 line
    case circle: Circle2 => this intersectCircle2 circle
    case _ => op intersect this
  }

  // ---- std ----

  override def toString: String = s"Circle2($sp, $range)"

  override def equals(op: Any) = op match {
    case circle: Circle2 => sp == circle.sp && range == circle.range
    case _ => false
  }

  override def hashCode: Int = 32*sp.## + range.##

  // ---- copy from Range2 ----

  def power: Double = sp unto range.power
  def powerSqr: Double = sp unto range.powerSqr
  def center: Point2 = sp unto range.center
  def isConcentric(circle: Circle2): Boolean = sp conjugate range.isConcentric apply circle
  def radicalLine(circle: Circle2): Line2 = sp conjugate range.radicalLine apply circle

  def through(pt: Point2): Boolean = sp conjugate range.through apply pt
  def containPoint2(pt: Point2): Boolean = sp conjugate range.containPoint2 apply pt
  def distance(pt: Point2): Double = sp conjugate range.distance apply pt
  def distanceSqr(pt: Point2): Double = sp conjugate range.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = sp conjugate range.nearest apply pt

  def sameCircle2(circle: Circle2): Boolean = sp conjugate range.sameCircle2 apply circle
  def aabb: AABB2 = sp unto range.aabb
  def isIntersectLine2(line: Line2): Boolean = sp conjugate range.isIntersectLine2 apply line
  def isIntersectCircle2(circle: Circle2): Boolean = sp conjugate range.isIntersectCircle2 apply circle
  def intersectLine2(line: Line2): Set[Point2] = range.intersectLine2(line from sp) map {_ unfrom sp}
  def intersectCircle2(circle: Circle2): Set[Point2] = range.intersectCircle2(circle from sp) map {_ unfrom sp}
}
