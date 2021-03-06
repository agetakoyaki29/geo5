package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.geometry._
import dim2.{Dim2Factory, Dim2, Vector2}


object Corner2 extends Dim2Factory[Corner2] {
  def apply(x: Double, y: Double): Corner2 = new Corner2(x, y)

  def Whole: Corner2 = new Corner2(Double.MaxValue, Double.MaxValue) {
    override def containPoint2(pt: Point2): Boolean = true
    override def toString: String = "Whole"
  }
}


@SameRet
class Corner2(_x: Double, _y: Double) extends Vector2(_x.abs, _y.abs) {

  override val factory: Dim2Factory[_ <: Corner2] = Corner2

  // ----

  def center: Point2 = O
  def minmin: Point2 = Point2(-x, -y)
  def minmax: Point2 = Point2(-x,  y)
  def maxmin: Point2 = Point2( x, -y)
  def maxmax: Point2 = Point2( x,  y)

  // def border(idx: Int): Border = ???
  // def borders: Seq[Border] = indices map {border _}
  // def bordersOther(border: Border): Seq[Border] = bordersOther(border.idx)
  // def bordersOther(idx: Int): Seq[Slab] = indicesOther(idx) map {border _}
  // def slab(idx: Int): Slab = ???
  // def slabsOther(slab: Slab): Seq[Slab] = slabsOther(slab.idx)
  // def slabsOther(idx: Int): Seq[Slab] = indicesOther(idx) map {slab _}
  // def slabs: Seq[Slab] = indices map {slab _}

  def isConcentric(aabb: AABB2): Boolean = center samePoint2 aabb.center

  // ---- figure to point ----

  def through(pt: Point2): Boolean = {
    if(pt.x.abs =~ x) pt.y.abs <~ y else
    if(pt.y.abs =~ y) pt.x.abs <~ x else
    false
  }

  def containPoint2(pt: Point2): Boolean = this zip pt.abs forall tupled{_>~_}

  def distance(pt: Point2): Double = distanceSqr(pt).sqrt
  def distanceSqr(pt: Point2): Double = {
    val distance = -pt.abs + this
    if(distance forall {_ >= 0}) distance.min.sqr
    else distance filterNot {_ >= 0} map {_.sqr} sum
  }

  def nearest(pt: Point2): Point2 = {
    def toIdxMap(seq: Seq[Double]): Map[Int, Double] = (seq.indices zip seq).toMap
    val distance = -pt.abs + this
    val ideaMap = toIdxMap(this zip pt map tupled{_ copySign _})
    if(distance forall {_ >= 0}) {
      val minElem = toIdxMap(distance) minBy {_._2}
      val update = ideaMap -- (ideaMap.keySet -  minElem._1)
      (pt /: update) {(p, t) => (p.updatedD2 _).tupled(t)}  // (pt /: update) {_ updated _}
    } else {
      val outMap = toIdxMap(distance) filterNot {_._2 >= 0}
      val update = ideaMap -- (ideaMap.keySet &~ outMap.keySet)
      (pt /: update) {(p, t) => (p.updatedD2 _).tupled(t)}  // (pt /: update) {_ updated _}
    }
  }

  // ---- figure to other figure ----

  def points: Set[Point2] = Set(O, minmin, minmax, maxmin, maxmax)

  def aabb: AABB2 = this

  def sameCorner2(op: Corner2): Boolean = this zip op forall tupled{_=~_}

  def sameAABB2(aabb: AABB2): Boolean = (this isConcentric aabb) && (this sameCorner2 aabb.corner)

  def isIntersectCircle2(circle: Circle2): Boolean = false  // TODO
  def intersectCircle2(circle: Circle2): Set[Point2] = Set()  // TODO

  def isIntersectAABB2(aabb: AABB2): Boolean = false  // TODO
  def intersectAABB2(aabb: AABB2): Set[Point2] = Set()  // TODO

  // ---- instead of SameRet ----

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


object AABB2 {
  def apply(sp: Point2, corner: Corner2) = new AABB2(sp, corner)
  def apply(sp: Point2, ep: Point2) = new AABB2(sp, Corner2(sp to ep))

  def Whole: AABB2 = new AABB2(O, Corner2.Whole) {
    override def updatedSP(sp: Point2): AABB2 = this
    override def contain(figure: Figure2): Boolean = true
    override def toString: String = "Whole"
  }

  implicit def corner2ToAABB2(corner: Corner2): AABB2 = AABB2(O, corner)
}


class AABB2(val sp: Point2, val corner: Corner2) extends Trans2[AABB2] with Figure2 {
  val ep = sp unto Point2(corner)

  def updated(sp: Point2, corner: Corner2): AABB2 = AABB2(sp, corner)
  def updatedSP(sp: Point2): AABB2 = updated(sp, corner)
  def updatedRange(corner: Corner2): AABB2 = updated(sp, corner)
  def updatedEP(ep: Point2): AABB2 = updatedRange(Corner2(sp to ep))

  // ---- for Trans2 ----

  def +(op: Point2): AABB2 = updatedSP(sp+op)
  def -(op: Point2): AABB2 = updatedSP(sp-op)

  // ---- for Figure2 ----

  def same(figure: Figure2): Boolean = figure match {
    case aabb: AABB2 => this sameAABB2 aabb
    case _ => false
  }

  def contain(op: Figure2): Boolean = op match {
    case line: Line2 => false
    case circle: Circle2 => (this containPoint2 circle.center) && ((this distanceSqr circle.center) <~ circle.powerSqr)
    case aabb: AABB2 => aabb.points map {this containPoint2 _} forall identity
  }

  def isIntersect(op: Figure2): Boolean = op match {
    case circle: Circle2 => this isIntersectCircle2 circle
    case aabb: AABB2     => this isIntersectAABB2 aabb
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Set[Point2] = op match {
    case circle: Circle2 => this intersectCircle2 circle
    case aabb: AABB2     => this intersectAABB2 aabb
    case _ => op intersect this
  }

  // ---- std ----

  override def toString: String = s"AABB2($sp, $corner)"

  override def equals(op: Any) = op match {
    case aabb: AABB2 => sp == aabb.sp && corner == aabb.corner
    case _ => false
  }

  override def hashCode: Int = 32*sp.## + corner.##

  // ---- copy from Corner2 ----

  def center: Point2 = sp unto corner.center
  def minmin: Point2 = sp unto corner.minmin
  def minmax: Point2 = sp unto corner.minmax
  def maxmin: Point2 = sp unto corner.maxmin
  def maxmax: Point2 = sp unto corner.maxmax
  def isConcentric(aabb: AABB2): Boolean = sp conjugate corner.isConcentric apply aabb

  def through(pt: Point2): Boolean = sp conjugate corner.through apply pt
  def containPoint2(pt: Point2): Boolean = sp conjugate corner.containPoint2 apply pt
  def distance(pt: Point2): Double = sp conjugate corner.distance apply pt
  def distanceSqr(pt: Point2): Double = sp conjugate corner.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = sp conjugate corner.nearest apply pt

  def points: Set[Point2] = corner.points map {_ unfrom sp}
  def aabb: AABB2 = sp unto corner.aabb
  def sameAABB2(aabb: AABB2): Boolean = sp conjugate corner.sameAABB2 apply aabb
  def isIntersectCircle2(circle: Circle2): Boolean = sp conjugate corner.isIntersectCircle2 apply circle
  def isIntersectAABB2(aabb: AABB2): Boolean = sp conjugate corner.isIntersectAABB2 apply aabb
  def intersectCircle2(circle: Circle2): Set[Point2] = corner.intersectCircle2(circle from sp) map {_ unfrom sp}
  def intersectAABB2(aabb: AABB2): Set[Point2] = corner.intersectAABB2(aabb from sp) map {_ unfrom sp}
}
